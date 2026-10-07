"""Image I/O, organism segmentation (GrabCut on CPU, or Segment Anything) and photo-quality metrics."""
from __future__ import annotations

import cv2
import numpy as np

_MODELS: dict = {}


def load_image(path, max_side: int | None = None) -> np.ndarray:
    """Read an image as RGB uint8 (works with non-ASCII Windows paths; EXIF orientation applied)."""
    bgr = cv2.imdecode(np.fromfile(str(path), dtype=np.uint8), cv2.IMREAD_COLOR)
    if bgr is None:
        raise IOError(f"Cannot read image: {path}")
    rgb = cv2.cvtColor(bgr, cv2.COLOR_BGR2RGB)
    return resize_max(rgb, max_side) if max_side else rgb


def resize_max(img: np.ndarray, max_side: int) -> np.ndarray:
    h, w = img.shape[:2]
    s = max_side / max(h, w)
    if s >= 1:
        return img
    return cv2.resize(img, (max(1, round(w * s)), max(1, round(h * s))), interpolation=cv2.INTER_AREA)


def clean_mask(mask: np.ndarray, min_frac: float = 0.002) -> np.ndarray:
    """Light opening, keep the largest connected component, fill interior holes."""
    m = mask.astype(np.uint8)
    r = max(1, int(round(0.003 * max(m.shape))))
    m = cv2.morphologyEx(m, cv2.MORPH_OPEN, cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (2 * r + 1, 2 * r + 1)))
    n, lab, stats, _ = cv2.connectedComponentsWithStats(m, connectivity=8)
    if n <= 1:
        return np.zeros(mask.shape, bool)
    m = (lab == 1 + int(np.argmax(stats[1:, cv2.CC_STAT_AREA]))).astype(np.uint8)
    cnts, _ = cv2.findContours(m, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
    out = np.zeros_like(m)
    cv2.drawContours(out, cnts, -1, 1, thickness=cv2.FILLED)
    out = out.astype(bool)
    return out if out.mean() >= min_frac else np.zeros_like(out)


def grabcut_mask(img, box=None, margin=0.05, iters=4, work_side=320) -> np.ndarray:
    """CPU segmentation. box=(x1, y1, x2, y2); default = whole frame minus `margin` (suits tight crops).
    GrabCut runs at `work_side` px for speed and the mask is upsampled to the input size."""
    H, W = img.shape[:2]
    s = min(1.0, work_side / max(H, W))
    if s < 1:
        small = cv2.resize(img, (max(1, round(W * s)), max(1, round(H * s))), interpolation=cv2.INTER_AREA)
        m = grabcut_mask(small, None if box is None else [v * s for v in box], margin, iters, work_side=10 ** 9)
        m = cv2.resize(m.astype(np.float32), (W, H), interpolation=cv2.INTER_LINEAR) > 0.5
        return clean_mask(m)
    h, w = H, W
    if box is None:
        box = (w * margin, h * margin, w * (1 - margin), h * (1 - margin))
    x1, y1, x2, y2 = (int(round(v)) for v in box)
    x1, y1 = max(0, x1), max(0, y1)
    x2, y2 = min(w, x2), min(h, y2)
    m = np.zeros((h, w), np.uint8)
    bgd, fgd = np.zeros((1, 65)), np.zeros((1, 65))
    try:
        cv2.grabCut(cv2.cvtColor(img, cv2.COLOR_RGB2BGR), m, (x1, y1, max(1, x2 - x1), max(1, y2 - y1)),
                    bgd, fgd, iters, cv2.GC_INIT_WITH_RECT)
    except cv2.error:
        return np.zeros((h, w), bool)
    return clean_mask((m == cv2.GC_FGD) | (m == cv2.GC_PR_FGD))


def sam_mask(img, box=None, weights="sam2.1_b.pt", margin=0.03) -> np.ndarray:
    """Segment Anything via Ultralytics (pip install ultralytics), prompted with a box."""
    from ultralytics import SAM
    if weights not in _MODELS:
        _MODELS[weights] = SAM(weights)
    h, w = img.shape[:2]
    if box is None:
        box = (w * margin, h * margin, w * (1 - margin), h * (1 - margin))
    r = _MODELS[weights](cv2.cvtColor(img, cv2.COLOR_RGB2BGR), bboxes=[[float(v) for v in box]], verbose=False)[0]
    if r.masks is None or len(r.masks.data) == 0:
        return np.zeros((h, w), bool)
    m = r.masks.data[0].cpu().numpy().astype(np.uint8)
    if m.shape != (h, w):
        m = cv2.resize(m, (w, h), interpolation=cv2.INTER_NEAREST)
    return clean_mask(m.astype(bool))


def segment(img, method="grabcut", box=None, margin=0.05, **kw) -> np.ndarray:
    if method == "grabcut":
        return grabcut_mask(img, box, margin, kw.get("iters", 4), kw.get("work_side", 320))
    if method == "sam":
        return sam_mask(img, box, kw.get("weights", "sam2.1_b.pt"))
    raise ValueError(f"unknown segmentation method: {method}")


def quality_metrics(img, mask) -> dict:
    """Photo/mask quality descriptors for filtering (not biological traits)."""
    h, w = mask.shape
    n = int(mask.sum())
    q = {"img_w": w, "img_h": h, "mask_px": n, "mask_frac": n / float(h * w), "border_touch": np.nan,
         "sharpness": np.nan, "clip_hi": np.nan, "clip_lo": np.nan, "solidity": np.nan, "equiv_diam_px": 0.0}
    if n == 0:
        return q
    q["border_touch"] = float(np.concatenate([mask[0], mask[-1], mask[:, 0], mask[:, -1]]).mean())
    gray = cv2.cvtColor(img, cv2.COLOR_RGB2GRAY).astype(np.float32)
    inner = cv2.erode(mask.astype(np.uint8), np.ones((5, 5), np.uint8)).astype(bool)
    if inner.sum() > 20:
        q["sharpness"] = float(cv2.Laplacian(gray, cv2.CV_32F)[inner].var())
    mx = img[mask].max(axis=1)
    q["clip_hi"], q["clip_lo"] = float((mx >= 250).mean()), float((mx <= 8).mean())
    cnts, _ = cv2.findContours(mask.astype(np.uint8), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
    c = max(cnts, key=cv2.contourArea)
    q["solidity"] = float(cv2.contourArea(c) / max(cv2.contourArea(cv2.convexHull(c)), 1.0))
    q["equiv_diam_px"] = float(2 * np.sqrt(n / np.pi))
    return q
