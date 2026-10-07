# InsectTrait

**Colour, pattern and shape traits of insects from photographs**

`InsectTrait` measures colour, pattern and shape traits of insects in field
photographs. It finds the insect, outlines it, decides whether the photo can be
used, checks colour and exposure, and returns one row of traits per photo with
quality metrics and a catalogue of how reliable each trait is. Photos can come
from your own folders or from GBIF and iNaturalist.

**Insects only.** The detector prompt, photo filter and trait catalogue were
developed and validated on 780 photos of 48 insect species (Coleoptera,
Diptera, Hymenoptera and Lepidoptera). The package is not intended for birds,
reptiles, plants or other organisms: `in_gbif_photos()` and `in_inat_photos()`
refuse taxa outside the class Insecta, `in_taxonomy()` warns about non-insect
species folders, and `in_process()` warns when the prompt is not an insect term.

## Installation

```r
install.packages("remotes")
remotes::install_github("ehsanrahimi666/InsectTrait")
```

Most functions run in R alone. Detection, outlining, the photo filter and the
57 extended traits need the optional Python back end, installed once:

```r
library(InsectTrait)
in_install()                 # or in_install(cpu_only = TRUE) on a laptop
in_use_backend()             # at the start of each R session
in_backend_available()       # TRUE when ready
```

The first run downloads the model weights (about 1.5 GB). The back end runs in
a separate Python process, so PyTorch is never loaded into R.

## Where the photos come from

```r
# one photo
res <- in_process("PATH_TO_ONE_PHOTO.jpg", out_dir = "PATH_TO_OUTPUT_FOLDER")

# a folder of photos (searched recursively)
res <- in_process("PATH_TO_PHOTO_FOLDER", out_dir = "PATH_TO_OUTPUT_FOLDER")

# a folder with one sub-folder per species, e.g. photos/Apis_mellifera/*.jpg
ph  <- in_read_folder("PATH_TO_SPECIES_FOLDERS")   # species from folder names
ph  <- in_taxonomy(ph)                             # adds order and family (GBIF)
res <- in_process(ph, out_dir = "PATH_TO_OUTPUT_FOLDER")

# GBIF or iNaturalist (CC0 and CC BY photos by default)
ph  <- in_gbif_photos("Aglais io", n = 100)
ph  <- in_inat_photos("Aglais io", n = 100)
ph  <- in_download_photos(ph, dir = "PATH_TO_PHOTO_FOLDER")
res <- in_process(ph, out_dir = "PATH_TO_OUTPUT_FOLDER")
writeLines(in_attribution(ph), "attribution.txt")
```

The `order` column (from `in_taxonomy()`, GBIF or iNaturalist) enables the
rule that reports the wing side of butterflies and moths.

## Quick start (runs as-is, no Python needed)

```r
library(InsectTrait)
ex <- in_example_photos()                       # 7 CC0 / CC BY photos
in_plot_outline(ex$crop[1], ex$mask[1], main = ex$species[1])
in_traits(ex$crop, ex$mask)                     # core traits and outline QC in R
res <- in_example_results()                     # back-end results, default rule
summary(res)
in_contact_sheet(res, ncol = 4)
in_trait_info(verdict = "not reliable")
```

## Colour checks and augmentation

```r
in_colour_check(ex$crop, ex$mask)               # colour cast, clipping, exposure
wb <- in_white_balance(ex$crop[3], "gray_pixels")
in_traits(ex$crop, ex$mask, white_balance = "gray_pixels")

img <- in_read_image(ex$crop[1]); m <- in_read_mask(ex$mask[1])
a <- in_augment(img, m, "hue", value = 0.05)    # rotate90, flip, brightness, contrast,
                                                # saturation, hue, white_balance, ...
in_augment_check(ex$crop[1], ex$mask[1])        # trait change under each augmentation
```

`in_augment_check()` applies the rotations, mirroring and colour jitter used to
augment training images and reports how far each trait moves, relative to the
usual difference between photos. In 100 validation photos, quarter turns and
mirroring left the traits unchanged, whereas training-style colour jitter moved
lightness, contrast and chroma traits by a quarter to almost half of that
difference, so traits should be measured on unmodified photos. Colour correction
is optional and off by default: in 111 validation pairs, no method improved
repeatability consistently (`in_benchmark$white_balance`).

## Decision rules

`in_filter()` applies one of the rules compared on 480 benchmark photos:

| Rule | Checks | Photos kept | Wrong outlines among kept |
|---|---|---|---|
| none | | 100% | 4.2% |
| `qc` | outline QC | 99% | 3.6% |
| `default` | outline QC, one insect, photo filter | 71% | 0.9% |
| `strict` | default + outline stability ≥ 0.8 | 56% | 0.4% |
| `top_view` | strict + top view and upper wing side | 40% | 0.5% |

## Functions

| Task | Functions |
|---|---|
| Photos | `in_read_folder()`, `in_taxonomy()`, `in_gbif_photos()`, `in_inat_photos()`, `in_download_photos()`, `in_attribution()`, `in_example_photos()` |
| Back end | `in_install()`, `in_use_backend()`, `in_backend_available()`, `in_process()`, `in_read_results()`, `in_extended_traits()`, `in_example_results()` |
| Traits in R | `in_read_image()`, `in_read_mask()`, `in_write_mask()`, `in_rgb_to_lab()`, `in_traits()` |
| Colour and augmentation | `in_colour_check()`, `in_white_balance()`, `in_augment()`, `in_augment_check()` |
| Decisions | `in_filter()`, `summary()` and `plot()` methods |
| Reliability | `in_trait_info()`, `in_reliable_traits()`, `in_icc()`, `in_repeatability()`, `in_invariance()` |
| Summaries and plots | `in_summarise()`, `in_plot_outline()`, `in_contact_sheet()`, `in_plot_traits()`, `in_plot_repeatability()` |
| Data | `in_catalogue`, `in_validation`, `in_benchmark` |

See `vignette("InsectTrait")` for a worked example.

## Limitations

- The visual labels and outline ratings used to train and evaluate the photo
  filter and decision rules were not compared with independent human labels
  (see the package article).
- Repeatability comes from two photos of the same observation; variation across
  days, observers and cameras will be larger.
- Validation covered 48 mostly European pollinator species from iNaturalist.
- SAM 2.1 is loaded through ultralytics, which is licensed under AGPL-3.0.

## License

GPL (>= 3). The example photos keep their own licences (CC0 1.0 or CC BY 4.0);
see `in_example_photos()` and `inst/extdata/ATTRIBUTION.csv`.
