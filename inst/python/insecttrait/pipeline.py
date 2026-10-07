"""End-to-end trait extraction with a resumable command-line interface.

Examples
--------
python -m insecttrait.pipeline --images D:/photos --out traits.csv --masks-dir masks --sheet qc.jpg
python -m insecttrait.pipeline --list crops_index.csv --path-col crop --root /kaggle/input/pollinator-crops-v1/crops \
       --keep species,occurrenceKey,file --seg sam --workers 1 --out traits.csv --masks-dir masks --sheet qc.jpg
"""
from __future__ import annotations

import argparse
import os
import time
from concurrent.futures import ProcessPoolExecutor
from functools import lru_cache
from pathlib import Path

import cv2
import numpy as np
import pandas as pd

from .colour import colour_metrics
from .pattern import pattern_metrics, shape_metrics
from .segment import load_image, quality_metrics, segment

DEFAULTS = dict(max_side=768, seg="grabcut", margin=0.05, cc="none", k=6, merge_de=10.0, min_px=600)
IMG_EXT = {".jpg", ".jpeg", ".png", ".tif", ".tiff", ".webp", ".bmp"}


def extract_traits(img, mask, cfg=None) -> dict:
    """All traits for one photo + organism mask (QC fields, colour, pattern, shape)."""
    cfg = {**DEFAULTS, **(cfg or {})}
    rec = quality_metrics(img, mask)
    if rec["mask_px"] < cfg["min_px"]:
        rec["status"] = "mask_too_small"
        return rec
    col, aux = colour_metrics(img, mask, cfg["cc"], cfg["k"], cfg["merge_de"])
    rec.update(col)
    rec.update(pattern_metrics(aux["L"], mask, aux["labels"], rec["equiv_diam_px"]))
    rec.update(shape_metrics(mask))
    rec["status"] = "ok"
    return rec


@lru_cache(maxsize=1)
def trait_fields() -> tuple:
    """Canonical output columns (from a synthetic two-colour disk), so chunked CSVs always align."""
    img = np.full((160, 160, 3), (90, 120, 60), np.uint8)
    mask = np.zeros((160, 160), np.uint8)
    cv2.circle(mask, (80, 80), 55, 1, -1)
    img[mask > 0] = (200, 120, 30)
    img[(mask > 0) & (np.arange(160)[None, :] < 80)] = (40, 30, 25)
    return tuple(extract_traits(img, mask.astype(bool)).keys()) + ("sec",)


def mask_path(masks_dir, img_path):
    p = Path(img_path)
    return None if masks_dir is None else os.path.join(masks_dir, f"{p.parent.name}__{p.stem}.png")


def process_image(path, cfg=None, mpath=None) -> dict:
    cfg = {**DEFAULTS, **(cfg or {})}
    try:
        img = load_image(path, cfg["max_side"])
        mask = segment(img, cfg["seg"], None, cfg["margin"])
        rec = extract_traits(img, mask, cfg)
        if mpath and mask.any():
            os.makedirs(os.path.dirname(mpath), exist_ok=True)
            cv2.imencode(".png", mask.astype(np.uint8) * 255)[1].tofile(mpath)
        return rec
    except Exception as e:  # keep long batch runs alive; the error is recorded per image
        return {"status": f"error: {type(e).__name__}: {e}"}


def _job(args):
    path, cfg, mpath = args
    t0 = time.time()
    rec = process_image(path, cfg, mpath)
    rec["sec"] = round(time.time() - t0, 3)
    return rec


def run(table, cfg, out_csv, masks_dir=None, workers=1, path_col="path", chunk=500) -> pd.DataFrame:
    """Process `table[path_col]`; appends to `out_csv` chunk by chunk and resumes if it exists."""
    cfg = {**DEFAULTS, **cfg}
    done = set()
    if os.path.exists(out_csv):
        done = set(pd.read_csv(out_csv, usecols=[path_col])[path_col].astype(str))
    todo = table[~table[path_col].astype(str).isin(done)].reset_index(drop=True)
    cols = list(table.columns) + [c for c in trait_fields() if c not in table.columns]
    ex = ProcessPoolExecutor(workers) if workers > 1 and cfg["seg"] != "sam" else None
    try:
        for s in range(0, len(todo), chunk):
            part = todo.iloc[s:s + chunk].reset_index(drop=True)
            jobs = [(p, cfg, mask_path(masks_dir, p)) for p in part[path_col]]
            res = list(ex.map(_job, jobs, chunksize=4)) if ex else [_job(j) for j in jobs]
            block = pd.concat([part, pd.DataFrame(res)], axis=1).reindex(columns=cols)
            block.to_csv(out_csv, mode="a", header=not os.path.exists(out_csv), index=False)
            print(f"{min(s + chunk, len(todo))}/{len(todo)} images processed", flush=True)
    finally:
        if ex:
            ex.shutdown()
    return pd.read_csv(out_csv)


def contact_sheet(table, masks_dir, out_path, path_col="path", label_col=None, n=48, tile=200, max_side=768, seed=0):
    """One JPEG of mask outlines for visual QC (send this single image for review, not raw photos)."""
    t = table.sample(min(n, len(table)), random_state=seed) if len(table) > n else table
    tiles = []
    for _, r in t.iterrows():
        try:
            img = load_image(r[path_col], max_side)
        except IOError:
            continue
        mp = mask_path(masks_dir, r[path_col])
        if mp and os.path.exists(mp):
            m = cv2.imdecode(np.fromfile(mp, np.uint8), cv2.IMREAD_GRAYSCALE)
            if m.shape != img.shape[:2]:
                m = cv2.resize(m, (img.shape[1], img.shape[0]), interpolation=cv2.INTER_NEAREST)
            cnts, _ = cv2.findContours((m > 0).astype(np.uint8), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
            img = cv2.drawContours(img.copy(), cnts, -1, (255, 230, 0), max(2, img.shape[1] // 160))
        s = tile / max(img.shape[:2])
        img = cv2.resize(img, (max(1, int(img.shape[1] * s)), max(1, int(img.shape[0] * s))), interpolation=cv2.INTER_AREA)
        canvas = np.full((tile, tile, 3), 255, np.uint8)
        canvas[:img.shape[0], :img.shape[1]] = img
        label = str(r[label_col])[:24] if label_col and label_col in r else Path(r[path_col]).stem[:24]
        cv2.putText(canvas, label, (3, tile - 6), cv2.FONT_HERSHEY_SIMPLEX, 0.4, (0, 0, 0), 1, cv2.LINE_AA)
        tiles.append(canvas)
    if not tiles:
        return None
    cols = 8
    rows = [np.hstack(tiles[i:i + cols] + [np.full((tile, tile, 3), 255, np.uint8)] * (cols - len(tiles[i:i + cols])))
            for i in range(0, len(tiles), cols)]
    cv2.imencode(".jpg", cv2.cvtColor(np.vstack(rows), cv2.COLOR_RGB2BGR), [cv2.IMWRITE_JPEG_QUALITY, 85])[1].tofile(out_path)
    return out_path


def main(argv=None):
    ap = argparse.ArgumentParser(description="Extract colour, pattern and shape traits from organism photos.")
    src = ap.add_mutually_exclusive_group(required=True)
    src.add_argument("--images", help="folder of images (searched recursively)")
    src.add_argument("--list", help="CSV with one row per image")
    ap.add_argument("--path-col", default="path")
    ap.add_argument("--root", default="", help="folder joined in front of --path-col values")
    ap.add_argument("--keep", default="", help="comma-separated --list columns copied to the output")
    ap.add_argument("--out", required=True)
    ap.add_argument("--masks-dir")
    ap.add_argument("--sheet", help="write a QC contact sheet (JPEG) of mask outlines")
    ap.add_argument("--seg", default="grabcut", choices=["grabcut", "sam"])
    ap.add_argument("--max-side", type=int, default=768)
    ap.add_argument("--cc", default="none", choices=["none", "gray_world", "shades_of_gray", "white_patch", "gray_edge"])
    ap.add_argument("--workers", type=int, default=max(1, (os.cpu_count() or 2) - 1))
    ap.add_argument("--chunk", type=int, default=500)
    ap.add_argument("--limit", type=int)
    a = ap.parse_args(argv)
    if a.images:
        pc = "path"
        table = pd.DataFrame({pc: sorted(str(p) for p in Path(a.images).rglob("*") if p.suffix.lower() in IMG_EXT)})
    else:
        pc = a.path_col
        t = pd.read_csv(a.list)
        table = t[[pc] + [c for c in a.keep.split(",") if c and c != pc]].copy()
        if a.root:
            table[pc] = [os.path.join(a.root, str(p)) for p in table[pc]]
    if a.limit:
        table = table.head(a.limit)
    cfg = {**DEFAULTS, "seg": a.seg, "max_side": a.max_side, "cc": a.cc}
    out = run(table, cfg, a.out, a.masks_dir, a.workers, pc, a.chunk)
    if a.sheet and a.masks_dir:
        contact_sheet(out[out["status"] == "ok"], a.masks_dir, a.sheet, pc, "species" if "species" in out else None)
    print(out["status"].astype(str).str.slice(0, 40).value_counts().to_string())


if __name__ == "__main__":
    main()
