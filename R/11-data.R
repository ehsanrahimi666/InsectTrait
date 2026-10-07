#' Trait catalogue: definitions and reliability of 67 insect traits
#'
#' Definitions and validation results for the 10 core and 57 extended traits
#' computed by the back end, from 780 'iNaturalist' photos of 48 insect species
#' (October 2026). Repeatability is the ICC(1) between two photos of the same
#' insect (195 observations with two photos), with 95% bootstrap intervals
#' (300 resamples of insects), for all pairs and for pairs in which both photos
#' passed a decision rule. Invariance is the ICC between the original crop and a
#' changed copy with the outline fixed (100 benchmark photos that passed the
#' strict rule).
#'
#' Verdict: `"reliable"` if `icc_strict >= 0.6`, `inv_photometric_min >= 0.8`
#' and `min_D_px <= 128`; `"use with care"` if `icc_strict >= 0.4` and
#' `inv_photometric_min >= 0.7`; otherwise `"not reliable"`.
#'
#' Two-photo pairs come from the same observation (taken seconds apart under
#' the same light), so repeatability across days, observers and cameras will be
#' lower.
#'
#' @format A data frame with 67 rows and 41 columns:
#' \describe{
#'   \item{trait, family, description}{Trait name, family (colour, pattern,
#'     shape) and definition. D is the insect's equivalent diameter.}
#'   \item{computed_by}{`"R and Python"` for traits that [in_traits()] also
#'     computes, `"Python"` for back-end traits.}
#'   \item{icc_all, icc_all_lo, icc_all_hi, n_all}{Repeatability (ICC), 95%
#'     interval and number of pairs, all pairs.}
#'   \item{icc_qc, icc_qc_lo, icc_qc_hi, n_qc}{The same for pairs passing
#'     outline QC with stability >= 0.8.}
#'   \item{icc_default, icc_default_lo, icc_default_hi, n_default}{The same
#'     for pairs passing the default rule of [in_filter()].}
#'   \item{icc_strict, icc_strict_lo, icc_strict_hi, n_strict}{The same for
#'     the strict rule; `icc_strict` is used for the verdict.}
#'   \item{icc_top, icc_top_lo, icc_top_hi, n_top}{The same for the top-view
#'     rule.}
#'   \item{inv_white_balance, inv_exposure, inv_jpeg, inv_blur, inv_rotate,
#'     inv_perspective}{Invariance ICC for each simulated change.}
#'   \item{inv_D32, inv_D64, inv_D128, inv_D256}{ICC between the original and a
#'     copy reduced so that the insect is 32-256 px across.}
#'   \item{inv_photometric_min, inv_geometric_min}{Smallest photometric
#'     (white balance, exposure, JPEG, blur) and geometric (rotation,
#'     perspective) invariance.}
#'   \item{min_D_px}{Smallest insect diameter on the ladder with ICC >= 0.8
#'     (`NA`: not reached at 256 px).}
#'   \item{species_icc_all, species_icc_filtered}{Share of variance among
#'     species (ICC(1) with species as groups, one photo per insect), for all
#'     photos and for photos passing the strict rule.}
#'   \item{verdict, reason}{Reliability verdict and, if not reliable, why.}
#' }
#' @source Validation of the \pkg{InsectTrait} pipeline; script
#'   `data-raw/make_datasets.R` in the package source.
#' @examples
#' table(in_catalogue$family, in_catalogue$verdict)
#' in_catalogue[in_catalogue$verdict != "reliable", c("trait", "verdict", "reason")]
#' @seealso [in_trait_info()], [in_reliable_traits()]
"in_catalogue"

#' Validation photos: quality metrics, photo filter, labels and traits
#'
#' One row per validation photo (780 research-grade 'iNaturalist' photos from
#' GBIF of 48 insect species in Coleoptera, Diptera, Hymenoptera and
#' Lepidoptera, processed by the back end on a CPU in October 2026). The table
#' holds no images; photos can be found from `occurrenceKey` on GBIF.
#'
#' `role`: `"benchmark"` (480 observations, 10 per species), `"pair_sibling"`
#' (88 second photos of benchmark observations) and `"pair_new"` (212 photos,
#' two from each of 106 more observations). Each of the 195 observations with
#' two photos forms a pair for repeatability.
#'
#' The 480 benchmark photos carry visual labels, and their outlines are rated
#' as good, minor error or wrong; the labelling procedure is described in the
#' package article. Label codes:
#' `label_view` D (top), O (oblique), L (side), F (front), V (below),
#' U (unclear); `label_wings` O (open), C (closed), P (partly open), "-" (not
#' Lepidoptera); `label_visibility` F (whole insect visible), P (partly hidden
#' or cut off), H (mostly hidden); `label_focus` S (sharp), B (blurred);
#' `label_lep_side` upper, under, mixed.
#'
#' @format A data frame with 780 rows and 109 columns:
#' \describe{
#'   \item{photo_id, occurrenceKey, role, order, family, species, country, year,
#'     license}{Photo and occurrence identifiers, taxonomy and photo licence.}
#'   \item{photo_w, photo_h}{Photo size used (longer side at most 2048 px).}
#'   \item{det1_score, n_other_insects, cut_by_frame}{Stage-1 detection score,
#'     number of other insects detected with a score of 0.30 or more, and
#'     whether the insect box touches the photo frame.}
#'   \item{det_score, sam_iou, stab, iou_stage1}{Stage-2 detection score, SAM
#'     predicted IoU, outline stability (smallest IoU with the jittered-box
#'     outlines) and IoU with the outline from the stage-1 box.}
#'   \item{px, mask_frac, border_touch, mask_ok, solidity, clip_hi, clip_lo,
#'     sharp, insect_px_native}{Outline quality metrics (see [in_traits()]);
#'     `insect_px_native` is the longer side of the stage-1 box in pixels.}
#'   \item{p_usable_any, p_top_view, p_upperside_lep, p_fully_visible,
#'     p_sharp}{Photo-filter probabilities.}
#'   \item{label_view, label_wings, label_visibility, label_focus,
#'     label_several, label_usable, label_usable_top, label_lep_side}{Visual
#'     labels (benchmark photos only).}
#'   \item{outline_rating}{Visual outline rating: good, minor error or wrong
#'     (benchmark photos only).}
#'   \item{D_px}{Equivalent diameter of the outline (pixels).}
#'   \item{L_mean, L_sd, C_mean, hue_deg, hue_conc, vap_wp, darkness_cw,
#'     dark_frac_otsu, L_median, L_p10, L_p90, L_range, a_mean,
#'     b_mean, C_p90, chrom_r, chrom_g, chrom_b, logchrom_rg,
#'     logchrom_bg, Y_mean, vap, dark_frac_L30, tone_bimodality,
#'     bg_dE00, bg_weber_Y, n_colour_classes, colour_shannon,
#'     colour_simpson, colour_contrast_dE, colour_max_dE,
#'     dark_class_frac, dark_class_L, dominant_frac, dominant_L,
#'     dominant_C, dominant_hue}{Colour traits; see [in_catalogue].}
#'   \item{L_entropy, edge_density, gran_D2, gran_D4, gran_D8, gran_D16,
#'     gran_D32, gran_D64, gran_total, gran_slope, gran_peak_D,
#'     gran_peak_prop, glcm_contrast, glcm_homogeneity, glcm_energy,
#'     glcm_correlation, pattern_fd, land_n_patches, land_LPI,
#'     land_mean_patch_frac, land_edge_density, land_AI, land_contag,
#'     moran_I, sym_pattern_r}{Pattern traits; see [in_catalogue].}
#'   \item{sym_mask_iou, shape_circularity, shape_solidity, shape_aspect,
#'     shape_roughness}{Shape traits; see [in_catalogue].}
#' }
#' @source Validation of the \pkg{InsectTrait} pipeline; script
#'   `data-raw/make_datasets.R`. Occurrence data: iNaturalist Research-grade
#'   Observations, GBIF dataset \doi{10.15468/ab3s5x}.
#' @examples
#' table(in_validation$order)
#' # wrong outlines among benchmark photos kept by each rule
#' b <- in_validation[in_validation$role == "benchmark", ]
#' sapply(c("qc", "default", "strict"), function(r) {
#'   k <- in_filter(b, r)$keep
#'   c(kept = mean(k), wrong = mean(b$outline_rating[k] == "wrong"))
#' })
"in_validation"

#' Benchmarks of detectors, segmenters, the photo filter, colour correction and augmentation
#'
#' `detectors` and `combinations`: a benchmark of zero-shot detectors and
#' box-prompted segmenters on 480 insect crops (10 per species, longer side
#' 768 px) run on a Tesla T4 GPU. Detectors used the prompt "insect" (`generic`) or six group names
#' (`groups`: bee, fly, butterfly, moth, beetle, wasp), a score threshold of
#' 0.05 and the central-box rule. The reference outline was the pixel-wise
#' majority of all combinations except 'GrabCut'; it is a consensus, not a
#' manual ground truth.
#'
#' `photo_filter`: cross-validated performance of the photo-filter heads
#' against the visual labels of the 480 benchmark photos (stratified 5-fold
#' cross-validation repeated twice; L2 logistic regression with the penalty
#' chosen by inner 3-fold cross-validation), for six feature sets and two
#' zero-shot baselines. The labels are described in [in_validation].
#'
#' `white_balance`: repeatability (ICC(1)) of the nine core traits between the
#' two photos of the validation pairs that passed the default rule, computed in
#' R after each colour correction of [in_white_balance()] (`gray_pixels_if_cast`:
#' grey-pixel correction only for photos with a colour cast above 0.2 in
#' [in_colour_check()]), with the difference from uncorrected traits and its
#' 95% paired bootstrap interval (1000 resamples of insects).
#'
#' `augmentation`: change of the core traits under the augmentations of
#' [in_augment()], from [in_augment_check()] on 100 benchmark photos that
#' passed the strict rule (one random draw per augmentation).
#'
#' @format A list of five data frames:
#' \describe{
#'   \item{detectors}{`detector`, `prompt`, `found` (share of crops with a
#'     detection), `median_score`, `sec_detect` (seconds per crop) and
#'     `errors`.}
#'   \item{combinations}{`detector`, `prompt`, `segmenter`, `n`, `mask_ok`
#'     (share of outlines passing QC), `iou_consensus` (mean IoU with the
#'     consensus outline), `trait_error` (mean absolute trait difference from
#'     the consensus in units of the trait's SD, for L_mean, C_mean, vap_wp,
#'     dark_frac_otsu and edge_density) and seconds per crop.}
#'   \item{photo_filter}{`question` (usable_any, top_view, upperside_lep,
#'     fully_visible, sharp), `features`, `n`, `positives`, `auc`, `auc_sd`
#'     (between repeats), `balanced_accuracy` (threshold 0.5) and `used`
#'     (the feature set of the head shipped with the back end).}
#'   \item{white_balance}{`method`, `trait`, `icc`, `diff` (ICC minus the ICC
#'     without correction), `lower` and `upper` (95% interval of `diff`),
#'     `n_pairs` and `change` (median absolute change of the trait over the
#'     780 validation crops, relative to the between-photo SD).}
#'   \item{augmentation}{`type`, `trait`, `median_rel_change` and
#'     `q90_rel_change` (median and 90th percentile of the change relative to
#'     the between-photo SD), `share_flagged` (share of photos with a relative
#'     change above 0.25) and `n_photos`.}
#' }
#' @source Segmentation benchmark of the \pkg{InsectTrait} project; script
#'   `data-raw/make_datasets.R`.
#' @examples
#' in_benchmark$detectors
#' head(in_benchmark$combinations[, c("detector", "prompt", "segmenter",
#'                                    "iou_consensus", "mask_ok")], 8)
#' in_benchmark$photo_filter[in_benchmark$photo_filter$used, ]
#' wb <- in_benchmark$white_balance
#' xtabs(icc ~ trait + method, wb)
#' aug <- in_benchmark$augmentation
#' round(xtabs(median_rel_change ~ type + trait, aug), 2)
"in_benchmark"
