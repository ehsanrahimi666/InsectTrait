# InsectTrait 0.1.0

* First release (developed as "phototraits").
* Photos from a single file, a folder, a folder of species sub-folders
  (`in_read_folder()`, `in_taxonomy()`), GBIF (`in_gbif_photos()`) or the
  iNaturalist API (`in_inat_photos()`); non-insect taxa are refused.
* Optional Python back end: two-stage Grounding DINO detection, SAM 2.1
  outline with prompt stability, outline quality control, photo filter and
  10 core plus 57 extended traits (`in_process()`), run in a separate Python
  process; optional white-balance correction before traits.
* Native R computation of the core colour traits and outline quality metrics
  (`in_traits()`), matching the back end.
* Colour checks (`in_colour_check()`, `in_white_balance()`) and training-style
  augmentation with trait stability checks (`in_augment()`,
  `in_augment_check()`).
* Validated decision rules (`in_filter()`), trait catalogue (`in_catalogue`),
  repeatability and invariance tools, plots and summaries. Subsetting results
  keeps the decision rule.
* Evaluation of colour correction and augmentation on the validation photos
  (`in_benchmark$white_balance`, `in_benchmark$augmentation`); colour
  correction stays off by default because no method improved repeatability
  consistently.
