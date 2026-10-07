"""Extended traits for crops and outlines (called from R by in_extended_traits()).

python -m insecttrait.api --pairs pairs.csv --out extended.csv [--max-side 768]
pairs.csv columns: photo_id, image, mask (PNG, nonzero = insect).
"""
import argparse

import cv2
import numpy as np
import pandas as pd

from .pipeline import extract_traits


def _read(path, flag):
    buf = np.fromfile(str(path), np.uint8)
    img = cv2.imdecode(buf, flag) if buf.size else None
    if img is None:
        raise IOError(f"cannot read image: {path}")
    return img


def extended_traits(image_path, mask_path, max_side=768):
    """Extended trait set for one crop and its outline; the crop is resized to max_side
    (INTER_AREA, mask nearest neighbour), as in the validation."""
    img = cv2.cvtColor(_read(image_path, cv2.IMREAD_COLOR), cv2.COLOR_BGR2RGB)
    m = _read(mask_path, cv2.IMREAD_GRAYSCALE) > 0
    if m.shape != img.shape[:2]:
        raise ValueError("image and mask differ in size")
    s = max_side / max(img.shape[:2]) if max_side else 1.0
    if s < 1:
        img = cv2.resize(img, None, fx=s, fy=s, interpolation=cv2.INTER_AREA)
        m = cv2.resize(m.astype(np.uint8), (img.shape[1], img.shape[0]), interpolation=cv2.INTER_NEAREST) > 0
    return extract_traits(img, m)


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--pairs", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--max-side", type=int, default=768)
    a = ap.parse_args(argv)
    pairs = pd.read_csv(a.pairs, dtype=str)
    rows = []
    for r in pairs.itertuples():
        try:
            rec = extended_traits(r.image, r.mask, a.max_side)
        except Exception as e:  # one unreadable file should not stop a batch
            rec = {"status": f"error: {type(e).__name__}: {str(e)[:150]}"}
        rows.append({"photo_id": r.photo_id, **rec})
    pd.DataFrame(rows).to_csv(a.out, index=False)


if __name__ == "__main__":
    main()
