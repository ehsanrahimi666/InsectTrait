"""Colour traits: camera normalisation, colour spaces, physics-based invariants and indices.

Colour work is done in linear RGB (sRGB gamma removed), so ratios and illuminant corrections follow
the image-formation model p_c = f_c( sum_lambda E(l) S(l) Q_c(l) ). Under a diagonal (von Kries)
illuminant, ratios between pixels of the same photo cancel the illuminant; chromaticities cancel
intensity/shading; hue is largely invariant to shading and highlights (dichromatic model).
"""
from __future__ import annotations

import cv2
import numpy as np

EPS = 1e-6
_YW = np.array([0.2126, 0.7152, 0.0722], np.float32)  # Rec.709 luminance weights (linear RGB)


def srgb_to_linear(x):
    return np.where(x <= 0.04045, x / 12.92, ((x + 0.055) / 1.055) ** 2.4)


def linear_to_srgb(x):
    x = np.clip(x, 0, 1)
    return np.where(x <= 0.0031308, 12.92 * x, 1.055 * np.power(x, 1 / 2.4) - 0.055)


def estimate_illuminant(lin, method="none", p=6) -> np.ndarray:
    """Global illuminant (RGB, sums to 3). none | gray_world | shades_of_gray | white_patch | gray_pixels | gray_edge."""
    if method == "none":
        return np.ones(3, np.float32)
    px = lin.reshape(-1, 3)
    if method == "gray_world":
        e = px.mean(0)
    elif method == "shades_of_gray":  # Finlayson & Trezzi 2004
        e = np.power(np.power(px, p).mean(0), 1 / p)
    elif method == "white_patch":
        e = np.percentile(px, 99, axis=0)
    elif method == "gray_pixels":  # mean of near-neutral pixels (as in the R package); no correction if too few
        srgb = linear_to_srgb(lin)
        v, m = srgb.max(2), srgb.min(2)
        s = np.where(v > 0, (v - m) / np.maximum(v, EPS), 0)
        ok = (s < 0.2) & (v > 0.1) & (v < 0.98)
        if ok.sum() < max(50, 0.01 * ok.size):
            return np.ones(3, np.float32)
        e = lin[ok].mean(0)
    elif method == "gray_edge":  # van de Weijer et al. 2007
        g = cv2.GaussianBlur(lin, (0, 0), 1.0)
        mag = np.sqrt(cv2.Sobel(g, cv2.CV_32F, 1, 0) ** 2 + cv2.Sobel(g, cv2.CV_32F, 0, 1) ** 2)
        e = np.power(np.power(mag.reshape(-1, 3), p).mean(0), 1 / p)
    else:
        raise ValueError(method)
    e = np.maximum(e, EPS)
    return (3 * e / e.sum()).astype(np.float32)


def ciede2000(lab1, lab2):
    """CIEDE2000 colour difference (Sharma et al. 2005); inputs broadcastable arrays (..., 3)."""
    lab1, lab2 = np.asarray(lab1, float), np.asarray(lab2, float)
    L1, a1, b1 = lab1[..., 0], lab1[..., 1], lab1[..., 2]
    L2, a2, b2 = lab2[..., 0], lab2[..., 1], lab2[..., 2]
    Cb = (np.hypot(a1, b1) + np.hypot(a2, b2)) / 2
    G = 0.5 * (1 - np.sqrt(Cb ** 7 / (Cb ** 7 + 25.0 ** 7)))
    a1p, a2p = (1 + G) * a1, (1 + G) * a2
    C1p, C2p = np.hypot(a1p, b1), np.hypot(a2p, b2)
    h1p, h2p = np.degrees(np.arctan2(b1, a1p)) % 360, np.degrees(np.arctan2(b2, a2p)) % 360
    zero = (C1p * C2p) == 0
    dh = h2p - h1p
    dh = np.where(dh > 180, dh - 360, np.where(dh < -180, dh + 360, dh))
    dh = np.where(zero, 0, dh)
    dLp, dCp = L2 - L1, C2p - C1p
    dHp = 2 * np.sqrt(C1p * C2p) * np.sin(np.radians(dh / 2))
    Lbp, Cbp = (L1 + L2) / 2, (C1p + C2p) / 2
    hs = h1p + h2p
    hbp = np.where(np.abs(h1p - h2p) > 180, np.where(hs < 360, (hs + 360) / 2, (hs - 360) / 2), hs / 2)
    hbp = np.where(zero, hs, hbp)
    T = (1 - 0.17 * np.cos(np.radians(hbp - 30)) + 0.24 * np.cos(np.radians(2 * hbp))
         + 0.32 * np.cos(np.radians(3 * hbp + 6)) - 0.20 * np.cos(np.radians(4 * hbp - 63)))
    dtheta = 30 * np.exp(-(((hbp - 275) / 25) ** 2))
    RC = 2 * np.sqrt(Cbp ** 7 / (Cbp ** 7 + 25.0 ** 7))
    SL = 1 + 0.015 * (Lbp - 50) ** 2 / np.sqrt(20 + (Lbp - 50) ** 2)
    SC, SH = 1 + 0.045 * Cbp, 1 + 0.015 * Cbp * T
    RT = -np.sin(np.radians(2 * dtheta)) * RC
    return np.sqrt((dLp / SL) ** 2 + (dCp / SC) ** 2 + (dHp / SH) ** 2 + RT * (dCp / SC) * (dHp / SH))


def colour_classes(lab, fit_mask, assign_mask, k=6, merge_de=10.0, min_prop=0.03, max_px=20000, seed=0):
    """k-means in CIELAB, merge clusters closer than `merge_de` (CIEDE2000) and absorb classes
    covering < `min_prop` of the organism (cf. recolorize).

    Returns centroids (m, 3), area proportions (m,) and a label map (-1 outside `assign_mask`)."""
    X = lab[fit_mask].astype(np.float32)
    if len(X) > max_px:
        X = X[np.random.default_rng(seed).choice(len(X), max_px, replace=False)]
    k = int(max(1, min(k, len(X) // 50)))
    if k > 1:
        cv2.setRNGSeed(seed)
        crit = (cv2.TERM_CRITERIA_EPS | cv2.TERM_CRITERIA_MAX_ITER, 50, 0.2)
        _, lbl, cen = cv2.kmeans(X, k, None, crit, 3, cv2.KMEANS_PP_CENTERS)
        w = np.bincount(lbl.ravel(), minlength=k).astype(float)
        cen = cen.astype(float)
    else:
        cen, w = X.mean(0, keepdims=True).astype(float), np.array([float(len(X))])
    cen, w = cen[w > 0], w[w > 0]
    while len(cen) > 1:
        d = ciede2000(cen[:, None, :], cen[None, :, :])
        np.fill_diagonal(d, np.inf)
        i, j = np.unravel_index(np.argmin(d), d.shape)
        if d[i, j] >= merge_de:
            break
        cen[i] = (cen[i] * w[i] + cen[j] * w[j]) / (w[i] + w[j])
        w[i] += w[j]
        cen, w = np.delete(cen, j, 0), np.delete(w, j)
    while len(cen) > 1 and (w / w.sum()).min() < min_prop:  # absorb tiny classes (edge/JPEG pixels)
        j = int(np.argmin(w))
        d = ciede2000(cen[j], cen)
        d[j] = np.inf
        i = int(np.argmin(d))
        cen[i] = (cen[i] * w[i] + cen[j] * w[j]) / (w[i] + w[j])
        w[i] += w[j]
        cen, w = np.delete(cen, j, 0), np.delete(w, j)
    P = lab[assign_mask].astype(np.float32)
    lbl = ((P[:, None, :] - cen[None, :, :].astype(np.float32)) ** 2).sum(-1).argmin(1)
    cnt = np.bincount(lbl, minlength=len(cen))
    keep = cnt > 0
    lbl = (np.cumsum(keep) - 1)[lbl]
    cen, props = cen[keep], cnt[keep] / cnt.sum()
    labels = np.full(assign_mask.shape, -1, np.int16)
    labels[assign_mask] = lbl
    return cen, props, labels


def colour_metrics(img, mask, cc="none", k=6, merge_de=10.0):
    """Colour traits of the organism (`mask`) in an RGB uint8 photo. Returns (traits, aux)."""
    lin = srgb_to_linear(img.astype(np.float32) / 255.0).astype(np.float32)
    illum = estimate_illuminant(lin, cc)
    if cc != "none":
        lin = np.clip(lin / illum, 0.0, 1.0).astype(np.float32)
    lab = cv2.cvtColor(lin, cv2.COLOR_LRGB2Lab)  # D65 white
    Yimg = lin @ _YW
    v = lin.max(2)
    s = (v - lin.min(2)) / (v + EPS)
    clip = v >= 0.995
    gloss = mask & (s < 0.25) & (v > max(0.6, 3.0 * float(np.median(v[mask])))) & ~clip  # specular highlights
    use = mask & ~clip & ~gloss
    if use.sum() < 0.3 * mask.sum():  # white or over-exposed organisms: keep everything
        use = mask.copy()

    L, a, b = lab[..., 0][use], lab[..., 1][use], lab[..., 2][use]
    R, G, B = lin[..., 0][use], lin[..., 1][use], lin[..., 2][use]
    S, Y = R + G + B + EPS, Yimg[use]
    C, th = np.hypot(a, b), np.arctan2(b, a)
    cw = C.sum() + EPS
    hx, hy = (C * np.cos(th)).sum() / cw, (C * np.sin(th)).sum() / cw
    p10, p50, p90 = np.percentile(L, [10, 50, 90])
    y_white = float(np.percentile(Yimg, 99)) + EPS
    t = dict(
        cc_illum_r=float(illum[0]), cc_illum_b=float(illum[2]),
        clip_frac=float(clip[mask].mean()), gloss_frac=float(gloss[mask].mean()),
        L_mean=float(L.mean()), L_median=float(p50), L_sd=float(L.std()), L_p10=float(p10), L_p90=float(p90),
        L_range=float(p90 - p10), a_mean=float(a.mean()), b_mean=float(b.mean()),
        C_mean=float(C.mean()), C_p90=float(np.percentile(C, 90)),
        hue_deg=float(np.degrees(np.arctan2(hy, hx)) % 360), hue_concentration=float(np.hypot(hx, hy)),
        chrom_r=float((R / S).mean()), chrom_g=float((G / S).mean()), chrom_b=float((B / S).mean()),
        logchrom_rg=float(np.log((R + EPS) / (G + EPS)).mean()), logchrom_bg=float(np.log((B + EPS) / (G + EPS)).mean()),
        Y_mean=float(Y.mean()), vap=float(1 - Y.mean()), vap_wp=float(1 - min(1.0, Y.mean() / y_white)),
        dark_frac_L30=float((L < 30).mean()),
    )
    # Otsu split of lightness: melanised fraction and tone bimodality (between-class / total variance)
    L8 = np.clip(L * 2.55, 0, 255).astype(np.uint8).reshape(1, -1)
    thr, _ = cv2.threshold(L8, 0, 255, cv2.THRESH_BINARY | cv2.THRESH_OTSU)
    dark = L8.ravel() <= thr
    pd_ = float(dark.mean())
    t["L_otsu"], t["dark_frac_otsu"] = float(thr / 2.55), pd_
    t["tone_bimodality"] = (float(pd_ * (1 - pd_) * (L[dark].mean() - L[~dark].mean()) ** 2 / L.var())
                            if 0 < pd_ < 1 and L.var() > 0 else 0.0)
    # Centre-weighted darkness: pixels near the body centre weigh more (thermal relevance hypothesis)
    ys, xs = np.nonzero(mask)
    D = 2 * np.sqrt(len(ys) / np.pi)
    wgt = np.exp(-np.hypot(ys - ys.mean(), xs - xs.mean()) / D / 0.25)
    t["darkness_cw"] = float(((1 - Yimg[ys, xs]) * wgt).sum() / wgt.sum())
    # Contrast with the immediate background ring (conspicuousness / crypsis proxy)
    r = max(3, int(0.15 * D))
    ring = cv2.dilate(mask.astype(np.uint8), cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (2 * r + 1, 2 * r + 1))).astype(bool) & ~mask
    if ring.sum() >= 50:
        t["bg_dE00"] = float(ciede2000(np.array([L.mean(), a.mean(), b.mean()]), lab[ring].mean(0)))
        ybg = float(Yimg[ring].mean())
        t["bg_weber_Y"] = float((Y.mean() - ybg) / (ybg + EPS))
    else:
        t["bg_dE00"] = t["bg_weber_Y"] = np.nan
    # Colour classes (perceptually merged) -> diversity, contrast, melanic class
    lab_s = cv2.cvtColor(cv2.GaussianBlur(lin, (0, 0), 0.7), cv2.COLOR_LRGB2Lab)
    cen, props, labels = colour_classes(lab_s, use, mask, k, merge_de)
    m = len(cen)
    dm = ciede2000(cen[:, None, :], cen[None, :, :])
    iu = np.triu_indices(m, 1)
    pw = props[iu[0]] * props[iu[1]]
    di, dom = int(np.argmin(cen[:, 0])), int(np.argmax(props))
    t.update(
        n_colour_classes=m, colour_shannon=float(-(props * np.log(props)).sum()), colour_simpson=float(1 - (props ** 2).sum()),
        colour_contrast_dE=float((pw * dm[iu]).sum() / pw.sum()) if m > 1 else 0.0,
        colour_max_dE=float(dm[iu].max()) if m > 1 else 0.0,
        dark_class_frac=float(props[di]), dark_class_L=float(cen[di, 0]),
        dominant_frac=float(props[dom]), dominant_L=float(cen[dom, 0]), dominant_C=float(np.hypot(cen[dom, 1], cen[dom, 2])),
        dominant_hue=float(np.degrees(np.arctan2(cen[dom, 2], cen[dom, 1])) % 360),
    )
    return t, {"L": lab[..., 0], "labels": labels, "use": use}
