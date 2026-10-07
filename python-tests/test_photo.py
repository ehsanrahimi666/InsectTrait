"""Model-free tests for the Python back end (python python-tests/test_photo.py from the package root)."""
import os
import sys

import numpy as np

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "inst", "python"))
from insecttrait.photo import DEFAULTS, clean, iou, localize, pick_central, scale_box, square_crop, traits, prep  # noqa: E402
from insecttrait.photofilter import PhotoFilter  # noqa: E402


def test_scale_box_clips_to_image():
    b = scale_box([10, 10, 90, 90], 1.5, 100, 100)
    assert b[0] == 0 and b[1] == 0 and b[2] == 100 and b[3] == 100
    b = scale_box([40, 40, 60, 60], 0.5, 100, 100)
    assert np.allclose(b, [45, 45, 55, 55])


def test_pick_central_prefers_centre():
    b = np.array([[0, 0, 20, 20], [40, 40, 60, 60]], float)
    s = np.array([0.9, 0.5])
    box, sc = pick_central(b, s, 100, 100)
    assert sc == 0.5 and box == [40.0, 40.0, 60.0, 60.0]
    assert pick_central(np.zeros((0, 4)), np.zeros(0), 100, 100) == (None, 0.0)


def test_localize_counts_other_insects_and_frame_cut():
    b = np.array([[0, 10, 50, 60], [55, 55, 95, 95], [10, 20, 30, 40]], float)  # main touches left edge; one separate; one inside main
    s = np.array([0.9, 0.6, 0.7])
    box, score, others, cut = localize(b, s, 100, 100, DEFAULTS)
    assert score == 0.9 and others == 1 and cut


def test_square_crop_inside_photo():
    img = np.zeros((300, 400, 3), np.uint8)
    crop, (x0, y0, x1, y1), s = square_crop(img, [350, 10, 390, 60], DEFAULTS)
    assert 0 <= x0 < x1 <= 400 and 0 <= y0 < y1 <= 300 and s == 1.0
    assert crop.shape[0] == y1 - y0 and crop.shape[1] == x1 - x0


def test_clean_keeps_largest_component_and_fills_holes():
    m = np.zeros((100, 100), bool)
    m[20:80, 20:80] = True
    m[40:50, 40:50] = False  # hole
    m[2:5, 2:5] = True  # small blob
    c = clean(m, [15, 15, 85, 85])
    assert c[45, 45] and not c[3, 3] and iou(c, c) == 1.0


def test_traits_on_two_tone_disk():
    img = np.full((200, 200, 3), (80, 140, 60), np.uint8)
    yy, xx = np.mgrid[:200, :200]
    m = (yy - 100) ** 2 + (xx - 100) ** 2 < 70 ** 2
    img[m] = (220, 120, 30)
    img[m & (xx < 100)] = (30, 25, 20)
    t = traits(prep(img), m)
    assert 0.4 < t["dark_frac_otsu"] < 0.6 and t["C_mean"] > 10 and t["D_px"] > 130


def test_decide_rules():
    base = dict(mask_ok=True, stab=0.95, n_other_insects=0, p_usable_any=0.9, p_top_view=0.8, p_upperside_lep=0.2)
    assert PhotoFilter.decide(base)["keep"]
    assert PhotoFilter.decide(base, order="Lepidoptera")["view"] == "underside"
    assert not PhotoFilter.decide({**base, "n_other_insects": 2})["keep"]
    assert not PhotoFilter.decide({**base, "p_usable_any": 0.2})["keep"]
    assert PhotoFilter.decide({**base, "stab": 0.5})["keep"]  # default rule: no stability check
    assert not PhotoFilter.decide({**base, "stab": 0.5}, thresholds={"stab": 0.8})["keep"]  # strict rule


if __name__ == "__main__":
    for name, f in list(globals().items()):
        if name.startswith("test_"):
            f()
            print("ok", name)
