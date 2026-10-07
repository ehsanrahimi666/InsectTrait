"""Pattern (spatial) and shape traits inside the organism mask.

Scale-free by design: spatial scales are expressed relative to the organism's equivalent diameter
D = 2*sqrt(area/pi), and the work is done at D <= 256 px. Colour-class maps are treated as
landscapes, so FRAGSTATS-style metrics (patches, aggregation, contagion, edge density) apply.
"""
from __future__ import annotations

import cv2
import numpy as np

WORK_D = 256
GRAN_KEYS = [f"gran_D{2 ** (k + 1)}" for k in range(6)] + ["gran_total", "gran_slope", "gran_peak_D", "gran_peak_prop"]


def _nblur(x, m, sigma):
    """Normalised (mask-aware) Gaussian blur; large sigmas are computed on a downsampled grid."""
    if sigma > 8:
        f = sigma / 8.0
        size = (max(1, int(x.shape[1] / f)), max(1, int(x.shape[0] / f)))
        num = cv2.GaussianBlur(cv2.resize(x * m, size, interpolation=cv2.INTER_AREA), (0, 0), 8)
        den = cv2.GaussianBlur(cv2.resize(m, size, interpolation=cv2.INTER_AREA), (0, 0), 8)
        return cv2.resize(num / np.maximum(den, 1e-6), (x.shape[1], x.shape[0]), interpolation=cv2.INTER_LINEAR)
    num = cv2.GaussianBlur(x * m, (0, 0), sigma)
    den = cv2.GaussianBlur(m, (0, 0), sigma)
    return num / np.maximum(den, 1e-6)


def granularity(L, mask, D, n_bands=6):
    """Band-pass energy spectrum of lightness (granularity analysis, cf. Stoddard & Stevens 2010).

    gran_D{n}: RMS lightness contrast of markings at scale ~D/n; slope of log-energy vs log-frequency
    (steep negative = few large markings); peak scale and its share of total energy."""
    sig = [D / 2 ** k for k in range(1, n_bands + 2)]
    m = mask.astype(np.float32)
    x = L.astype(np.float32)
    inner = cv2.erode(mask.astype(np.uint8), np.ones((3, 3), np.uint8)).astype(bool)
    blurs = [_nblur(x, m, s) if s >= 0.7 else None for s in sig]
    out = dict.fromkeys(GRAN_KEYS, np.nan)
    e = np.full(n_bands, np.nan)
    for k in range(n_bands):
        if blurs[k] is not None and blurs[k + 1] is not None and inner.any():
            e[k] = np.sqrt(np.mean((blurs[k + 1] - blurs[k])[inner] ** 2))
        out[GRAN_KEYS[k]] = float(e[k])
    ok = np.isfinite(e) & (e > 0)
    out["gran_total"] = float(np.nansum(e))
    if ok.sum() >= 3:
        f = np.arange(1, n_bands + 1, dtype=float)  # log2 of frequency (cycles per D, up to a constant)
        out["gran_slope"] = float(np.polyfit(f[ok], np.log2(e[ok]), 1)[0])
        kpk = int(np.nanargmax(np.where(ok, e, np.nan)))
        out["gran_peak_D"] = float(2 ** (kpk + 1))
        out["gran_peak_prop"] = float(e[kpk] / e[ok].sum())
    return out


def edge_density(L, mask, D, lo=40, hi=100):
    """Internal lightness edges per body length (outline excluded)."""
    r = max(2, int(0.02 * D))
    inner = cv2.erode(mask.astype(np.uint8), cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (2 * r + 1, 2 * r + 1))).astype(bool)
    if inner.sum() < 50:
        return np.nan
    L8 = cv2.GaussianBlur(np.clip(L * 2.55, 0, 255).astype(np.uint8), (0, 0), max(1.0, D / 200))
    e = cv2.Canny(L8, lo, hi) > 0
    return float(e[inner].sum() / inner.sum() * D)


def lightness_entropy(L, mask, bins=50):
    h, _ = np.histogram(L[mask], bins=bins, range=(0, 100))
    p = h[h > 0] / h.sum()
    return float(-(p * np.log2(p)).sum())


def _pairs(Q, mask, dy, dx):
    h, w = Q.shape
    a = (slice(max(0, -dy), h - max(0, dy)), slice(max(0, -dx), w - max(0, dx)))
    b = (slice(max(0, dy), h - max(0, -dy)), slice(max(0, dx), w - max(0, -dx)))
    ok = mask[a] & mask[b]
    return Q[a][ok], Q[b][ok]


def glcm_features(L, mask, D, levels=8):
    """Haralick texture on within-organism lightness quantiles (independent of overall lightness)."""
    keys = ("glcm_contrast", "glcm_homogeneity", "glcm_energy", "glcm_correlation")
    qs = np.quantile(L[mask], np.linspace(0, 1, levels + 1)[1:-1])
    Q = np.digitize(L, qs).astype(np.int64)
    d = max(1, int(round(D / 32)))
    i, j = np.indices((levels, levels))
    feats = []
    for dy, dx in ((0, d), (d, d), (d, 0), (d, -d)):
        a, b = _pairs(Q, mask, dy, dx)
        if a.size < 50:
            continue
        P = np.bincount(a * levels + b, minlength=levels * levels).reshape(levels, levels).astype(float)
        P = P + P.T
        P /= P.sum()
        mi, si = (i * P).sum(), np.sqrt((P * (i - (i * P).sum()) ** 2).sum())
        feats.append([(P * (i - j) ** 2).sum(), (P / (1 + np.abs(i - j))).sum(), (P ** 2).sum(),
                      (P * (i - mi) * (j - mi)).sum() / (si * si + 1e-12)])
    if not feats:
        return dict.fromkeys(keys, np.nan)
    return dict(zip(keys, map(float, np.mean(feats, 0))))


def boundary_fractal_dim(labels, mask, D):
    """Box-counting dimension of colour-class boundaries (1 = smooth, ->2 = convoluted)."""
    b = np.zeros(mask.shape, bool)
    b[:, 1:] |= (labels[:, 1:] != labels[:, :-1]) & mask[:, 1:] & mask[:, :-1]
    b[1:, :] |= (labels[1:, :] != labels[:-1, :]) & mask[1:, :] & mask[:-1, :]
    ys, xs = np.nonzero(b)
    sizes = [2 ** j for j in range(1, int(np.log2(max(4.0, D / 4))) + 1)]
    if len(ys) < 50 or len(sizes) < 3:
        return np.nan
    counts = [len(np.unique((ys // s) * 1_000_003 + xs // s)) for s in sizes]
    return float(-np.polyfit(np.log(sizes), np.log(counts), 1)[0])


def landscape_metrics(labels, mask, D):
    """FRAGSTATS-style metrics of the colour-class map (He et al. 2000; Li & Reynolds 1993)."""
    out = dict(land_n_patches=0, land_LPI=np.nan, land_mean_patch_frac=np.nan, land_edge_density=np.nan,
               land_AI=np.nan, land_contag=np.nan)
    cls = np.unique(labels[mask])
    m, A = len(cls), float(mask.sum())
    if m == 0 or A == 0:
        return out
    G = np.zeros((m, m))
    for a, b in ((labels[:, :-1], labels[:, 1:]), (labels[:-1, :], labels[1:, :])):
        ok = (a >= 0) & (b >= 0)
        ai, bi = np.searchsorted(cls, a[ok]), np.searchsorted(cls, b[ok])
        g = np.bincount(ai * m + bi, minlength=m * m).reshape(m, m)
        G += g + g.T  # double-count method
    P = np.array([(labels[mask] == c).mean() for c in cls])
    aiv = []
    for i in range(m):
        n = P[i] * A
        s = int(np.floor(np.sqrt(n)))
        r = n - s * s
        gmax = 2 * s * (s - 1) + (0 if r == 0 else (2 * r - 1 if r <= s else 2 * r - 2))
        aiv.append(G[i, i] / 2 / gmax if gmax > 0 else np.nan)
    out["land_AI"] = float(np.nansum(P * np.array(aiv)))
    if m > 1:
        q = P[:, None] * G / np.maximum(G.sum(1, keepdims=True), 1)
        q = q[q > 0]
        out["land_contag"] = float(1 + (q * np.log(q)).sum() / (2 * np.log(m)))
    out["land_edge_density"] = float((G.sum() - np.trace(G)) / 2 / A * D)
    min_area = max(4, int((D / 50) ** 2))
    areas = []
    for c in cls:
        _, _, st, _ = cv2.connectedComponentsWithStats((labels == c).astype(np.uint8), connectivity=8)
        areas += [x for x in st[1:, cv2.CC_STAT_AREA] if x >= min_area]
    if areas:
        areas = np.asarray(areas, float)
        out.update(land_n_patches=int(len(areas)), land_LPI=float(areas.max() / A), land_mean_patch_frac=float(areas.mean() / A))
    return out


def morans_i(L, mask, D, cells=16):
    """Moran's I of lightness on a grid of cells ~D/16 (rook neighbours): spatial aggregation."""
    c = max(2, int(round(D / cells)))
    h, w = (L.shape[0] // c) * c, (L.shape[1] // c) * c
    if h < 3 * c or w < 3 * c:
        return np.nan
    Ls = (L * mask)[:h, :w].reshape(h // c, c, w // c, c).sum((1, 3))
    Ms = mask[:h, :w].reshape(h // c, c, w // c, c).sum((1, 3)).astype(float)
    x = np.where(Ms >= 0.5 * c * c, Ls / np.maximum(Ms, 1), np.nan)
    if np.isfinite(x).sum() < 9:
        return np.nan
    z = x - np.nanmean(x)
    num, W = 0.0, 0
    for a, b in ((z[:, :-1], z[:, 1:]), (z[:-1, :], z[1:, :])):
        ok = np.isfinite(a) & np.isfinite(b)
        num += float((a[ok] * b[ok]).sum())
        W += int(ok.sum())
    den = float(np.nansum(z ** 2))
    return float(np.isfinite(z).sum() * num / (W * den)) if W and den > 0 else np.nan


def symmetry(L, mask, max_px=40000, seed=0):
    """Bilateral symmetry about the better of the two principal axes.

    sym_mask_iou: outline symmetry (~1 for flat, orthogonal views -> a free 'flat dorsal view' filter);
    sym_pattern_r: correlation of lightness between mirrored pixels (pattern symmetry)."""
    out = dict(sym_mask_iou=np.nan, sym_pattern_r=np.nan, sym_axis_deg=np.nan)
    mu = cv2.moments(mask.astype(np.uint8), binaryImage=True)
    if mu["m00"] < 100:
        return out
    cx, cy = mu["m10"] / mu["m00"], mu["m01"] / mu["m00"]
    theta = 0.5 * np.arctan2(2 * mu["mu11"], mu["mu20"] - mu["mu02"])
    ys, xs = np.nonzero(mask)
    if len(ys) > max_px:
        sel = np.random.default_rng(seed).choice(len(ys), max_px, replace=False)
        ys, xs = ys[sel], xs[sel]
    best = None
    for phi in (theta, theta + np.pi / 2):
        ux, uy = np.cos(phi), np.sin(phi)
        dx, dy = xs - cx, ys - cy
        t = dx * ux + dy * uy
        rx = np.round(cx + 2 * t * ux - dx).astype(int)
        ry = np.round(cy + 2 * t * uy - dy).astype(int)
        inside = (rx >= 0) & (rx < mask.shape[1]) & (ry >= 0) & (ry < mask.shape[0])
        hit = np.zeros(len(xs), bool)
        hit[inside] = mask[ry[inside], rx[inside]]
        iou = hit.sum() / (2 * len(xs) - hit.sum())
        r = np.nan
        if hit.sum() > 50:
            va, vb = L[ys[hit], xs[hit]], L[ry[hit], rx[hit]]
            if va.std() > 0 and vb.std() > 0:
                r = float(np.corrcoef(va, vb)[0, 1])
        if best is None or iou > best[0]:
            best = (float(iou), r, float(np.degrees(phi) % 180))
    out.update(sym_mask_iou=best[0], sym_pattern_r=best[1], sym_axis_deg=best[2])
    return out


def _mode_filter(labels, mask, k):
    cls = np.unique(labels[mask])
    if len(cls) <= 1:
        return labels
    votes = np.stack([cv2.blur((labels == c).astype(np.float32), (k, k)) for c in cls])
    out = cls[np.argmax(votes, 0)].astype(labels.dtype)
    out[~mask] = -1
    return out


def pattern_metrics(L, mask, labels, D=None) -> dict:
    """All spatial-pattern traits. L: CIELAB lightness map; labels: colour-class map (-1 outside)."""
    ys, xs = np.nonzero(mask)
    D = D or 2 * np.sqrt(len(ys) / np.pi)
    pad = int(0.05 * D) + 2
    y0, y1 = max(0, ys.min() - pad), min(mask.shape[0], ys.max() + pad + 1)
    x0, x1 = max(0, xs.min() - pad), min(mask.shape[1], xs.max() + pad + 1)
    L, mask, labels = L[y0:y1, x0:x1].astype(np.float32), mask[y0:y1, x0:x1], labels[y0:y1, x0:x1]
    if D > WORK_D:
        s = WORK_D / D
        size = (max(8, int(round(L.shape[1] * s))), max(8, int(round(L.shape[0] * s))))
        L = cv2.resize(L, size, interpolation=cv2.INTER_AREA)
        mask = cv2.resize(mask.astype(np.float32), size, interpolation=cv2.INTER_AREA) > 0.5
        labels = cv2.resize(labels.astype(np.float32), size, interpolation=cv2.INTER_NEAREST).astype(np.int16)
        labels[~mask] = -1
        D = 2 * np.sqrt(mask.sum() / np.pi)
    labels = labels.copy()
    labels[mask & (labels < 0)] = 0
    lab_s = _mode_filter(labels, mask, max(3, int(D / 64) | 1))
    out = granularity(L, mask, D)
    out["edge_density"] = edge_density(L, mask, D)
    out["L_entropy"] = lightness_entropy(L, mask)
    out.update(glcm_features(L, mask, D))
    out["pattern_fd"] = boundary_fractal_dim(lab_s, mask, D)
    out.update(landscape_metrics(lab_s, mask, D))
    out["moran_I"] = morans_i(L, mask, D)
    out.update(symmetry(L, mask))
    return out


def shape_metrics(mask) -> dict:
    """Size-free outline descriptors (viewpoint-sensitive: interpret only for standardised views)."""
    out = dict(shape_circularity=np.nan, shape_solidity=np.nan, shape_aspect=np.nan, shape_roughness=np.nan)
    m8 = mask.astype(np.uint8)
    cnts, _ = cv2.findContours(m8, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
    if not cnts:
        return out
    c = max(cnts, key=cv2.contourArea)
    A, P = cv2.contourArea(c), cv2.arcLength(c, True)
    mu = cv2.moments(m8, binaryImage=True)
    cov = np.array([[mu["mu20"], mu["mu11"]], [mu["mu11"], mu["mu02"]]]) / max(mu["m00"], 1)
    ev = np.sort(np.linalg.eigvalsh(cov))[::-1]
    D = 2 * np.sqrt(mask.sum() / np.pi)
    sm = (cv2.GaussianBlur(m8.astype(np.float32), (0, 0), max(1.0, D / 40)) > 0.5).astype(np.uint8)
    cs, _ = cv2.findContours(sm, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
    Ps = cv2.arcLength(max(cs, key=cv2.contourArea), True) if cs else 0
    out.update(shape_circularity=float(4 * np.pi * A / P ** 2) if P > 0 else np.nan,
               shape_solidity=float(A / max(cv2.contourArea(cv2.convexHull(c)), 1.0)),
               shape_aspect=float(np.sqrt(ev[0] / max(ev[1], 1e-9))),
               shape_roughness=float(P / Ps) if Ps > 0 else np.nan)  # outline fuzziness (pilosity proxy)
    return out
