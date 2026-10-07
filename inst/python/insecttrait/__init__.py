"""insecttrait: colour, pattern and shape traits of insects from photographs (back end of the InsectTrait R package).

The core (traits from an image + mask) depends only on numpy, OpenCV and pandas, so it runs on a laptop,
on Kaggle, from R (reticulate) and can be ported to mobile.

Photo pipeline (validated Oct 2026 on 780 iNaturalist photos of 48 insect species):
    photo.py        photo -> Grounding DINO-base (two stages) -> native crop -> SAM 2.1-B outline -> QC -> core traits
    photofilter.py  is the photo usable? view (top/side), wing side (Lepidoptera), visibility, sharpness
    run_photos.py   batch CLI: folder or CSV of URLs -> traits.csv, crops/, masks/
These need torch, transformers, ultralytics and open_clip_torch (heavy imports are kept inside those modules).
"""
__version__ = "0.2.0"

from .segment import load_image, segment, quality_metrics  # noqa: F401
from .colour import colour_metrics, ciede2000  # noqa: F401
from .pattern import pattern_metrics, shape_metrics  # noqa: F401
from .pipeline import extract_traits, process_image  # noqa: F401
