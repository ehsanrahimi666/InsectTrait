# Builds data/in_catalogue.rda, data/in_benchmark.rda and data/in_validation.rda
# from the validation tables in data-raw/validation/ (outputs of the October 2026
# pipeline validation and segmentation benchmark). Run from the package root:
#   Rscript data-raw/make_datasets.R

src <- file.path("data-raw", "validation")
rd <- function(f, ...) utils::read.csv(file.path(src, f), check.names = FALSE,
                                       stringsAsFactors = FALSE, ...)

core <- c("L_mean", "L_sd", "C_mean", "hue_deg", "hue_conc", "vap_wp",
          "darkness_cw", "dark_frac_otsu", "L_entropy", "edge_density")

# ---- trait catalogue -------------------------------------------------------
cat0 <- rd("trait_catalogue.csv")
pair <- rd("pair_icc.csv")
inv <- rd("invariance_icc.csv")
names(inv)[1] <- "trait"
sp <- rd("species_icc.csv")
names(sp)[1] <- "trait"

family <- ifelse(grepl("^shape_", cat0$trait) | cat0$trait == "sym_mask_iou", "shape",
          ifelse(grepl("^(gran_|glcm_|land_|pattern_|moran|edge_)", cat0$trait) |
                   cat0$trait %in% c("L_entropy", "sym_pattern_r"), "pattern", "colour"))

desc <- c(
  L_mean = "Mean CIELAB lightness L* of insect pixels (0-100).",
  L_sd = "Standard deviation of L* across insect pixels.",
  C_mean = "Mean CIELAB chroma C*ab = sqrt(a*^2 + b*^2).",
  hue_deg = "Chroma-weighted circular mean hue angle h_ab (degrees).",
  hue_conc = "Length of the chroma-weighted mean hue vector (0-1); 1 means a single hue.",
  vap_wp = "1 - min(1, mean luminance Y of the insect / 99th percentile of Y in the crop); higher is darker.",
  darkness_cw = "Weighted mean of (1 - Y), weights exp(-d / 0.25D), d = distance to the mask centroid.",
  dark_frac_otsu = "Fraction of insect pixels at or below the Otsu threshold of L*.",
  L_entropy = "Shannon entropy (bits) of the L* histogram (50 bins over 0-100).",
  edge_density = "Canny edges (40/100) of L* inside the eroded mask per pixel, multiplied by D.",
  L_median = "Median L*.",
  L_p10 = "10th percentile of L*.",
  L_p90 = "90th percentile of L*.",
  L_range = "L_p90 - L_p10.",
  a_mean = "Mean a* (green-red axis).",
  b_mean = "Mean b* (blue-yellow axis).",
  C_p90 = "90th percentile of chroma.",
  chrom_r = "Mean chromaticity R / (R + G + B) in linear RGB.",
  chrom_g = "Mean chromaticity G / (R + G + B) in linear RGB.",
  chrom_b = "Mean chromaticity B / (R + G + B) in linear RGB.",
  logchrom_rg = "Mean log(R / G) in linear RGB.",
  logchrom_bg = "Mean log(B / G) in linear RGB.",
  Y_mean = "Mean relative luminance Y (linear RGB, Rec. 709 weights).",
  vap = "1 - Y_mean (same information as Y_mean).",
  dark_frac_L30 = "Fraction of insect pixels with L* < 30.",
  tone_bimodality = "Between-class / total variance of L* at the Otsu split (0-1).",
  bg_dE00 = "CIEDE2000 difference between the mean insect colour and a surrounding background ring (0.15D wide).",
  bg_weber_Y = "Weber luminance contrast (Y_insect - Y_ring) / Y_ring.",
  n_colour_classes = "Number of colour classes (k-means, k = 6, in CIELAB; classes closer than dE00 = 10 merged, classes < 3% absorbed).",
  colour_shannon = "Shannon diversity of colour-class areas.",
  colour_simpson = "Simpson diversity (1 - sum p^2) of colour-class areas.",
  colour_contrast_dE = "Area-weighted mean pairwise CIEDE2000 difference between colour classes.",
  colour_max_dE = "Largest pairwise CIEDE2000 difference between colour classes.",
  dark_class_frac = "Area share of the darkest colour class.",
  dark_class_L = "L* of the darkest colour class.",
  dominant_frac = "Area share of the largest colour class.",
  dominant_L = "L* of the largest colour class.",
  dominant_C = "Chroma of the largest colour class.",
  dominant_hue = "Hue angle of the largest colour class (degrees).",
  gran_D2 = "Band-pass RMS lightness contrast at scale about D/2 (granularity).",
  gran_D4 = "Band-pass RMS lightness contrast at scale about D/4.",
  gran_D8 = "Band-pass RMS lightness contrast at scale about D/8.",
  gran_D16 = "Band-pass RMS lightness contrast at scale about D/16.",
  gran_D32 = "Band-pass RMS lightness contrast at scale about D/32.",
  gran_D64 = "Band-pass RMS lightness contrast at scale about D/64.",
  gran_total = "Sum of the six granularity band energies.",
  gran_slope = "Slope of log2 band energy against log2 spatial frequency.",
  gran_peak_D = "Scale n (in D/n) of the band with the highest energy.",
  gran_peak_prop = "Share of the total band energy in the peak band.",
  glcm_contrast = "GLCM contrast of L* quantised to 8 within-insect quantile levels (offset about D/32, 4 directions).",
  glcm_homogeneity = "GLCM homogeneity (same quantisation and offsets).",
  glcm_energy = "GLCM energy (angular second moment).",
  glcm_correlation = "GLCM correlation.",
  pattern_fd = "Box-counting fractal dimension of colour-class boundaries.",
  land_n_patches = "Number of colour-class patches (8-connected, area >= (D/50)^2 px).",
  land_LPI = "Largest patch area / insect area.",
  land_mean_patch_frac = "Mean patch area / insect area.",
  land_edge_density = "Length of colour-class boundaries per insect area, multiplied by D.",
  land_AI = "Aggregation index of the colour-class map, area-weighted over classes.",
  land_contag = "Contagion index of the colour-class map.",
  moran_I = "Moran's I of L* on a grid of cells about D/16 wide (rook neighbours).",
  sym_mask_iou = "IoU between the outline and its mirror image about the better principal axis.",
  sym_pattern_r = "Pearson correlation of L* between mirrored pixel pairs.",
  shape_circularity = "4 pi A / P^2 of the outline.",
  shape_solidity = "Outline area / convex hull area.",
  shape_aspect = "Square root of the ratio of the two principal second moments of the mask.",
  shape_roughness = "Outline perimeter / perimeter of the Gaussian-smoothed outline."
)
stopifnot(setequal(names(desc), cat0$trait))

pick <- function(d, cols, new) {
  out <- d[match(cat0$trait, d$trait), cols, drop = FALSE]
  names(out) <- new
  out
}
in_catalogue <- data.frame(
  trait = cat0$trait, family = family, description = unname(desc[cat0$trait]),
  computed_by = ifelse(cat0$trait %in% setdiff(core, "edge_density"), "R and Python", "Python"),
  stringsAsFactors = FALSE)
lv <- c(all = "all pairs", qc = "mask QC pass", default = "photo filter, no stability",
        strict = "photo filter pass", top = "top view only")
for (k in names(lv)) {
  in_catalogue <- cbind(in_catalogue, pick(pair, paste0(lv[[k]], c("", " lo", " hi", " n")),
                                           paste0(c("icc_", "icc_", "icc_", "n_"), k, c("", "_lo", "_hi", ""))))
}
pert <- c("white_balance", "exposure", "jpeg", "blur", "rotate", "perspective", "D32", "D64", "D128", "D256")
in_catalogue <- cbind(in_catalogue, pick(inv, pert, paste0("inv_", pert)))
in_catalogue$inv_photometric_min <- cat0$invariance_min_photometric
in_catalogue$inv_geometric_min <- cat0$invariance_min_geometric
in_catalogue$min_D_px <- as.integer(cat0$min_D_px)
in_catalogue <- cbind(in_catalogue, pick(sp, c("species ICC (all photos)", "species ICC (filter pass)"),
                                         c("species_icc_all", "species_icc_filtered")))
in_catalogue$verdict <- factor(cat0$verdict, levels = c("reliable", "use with care", "not reliable"))
in_catalogue$reason <- ifelse(is.na(cat0$why_not_reliable), "", cat0$why_not_reliable)
in_catalogue$reason[in_catalogue$trait == "vap"] <- "duplicates Y_mean (vap = 1 - Y_mean)"
num <- vapply(in_catalogue, is.double, logical(1))
in_catalogue[num] <- lapply(in_catalogue[num], round, 3)
for (n in grep("^n_", names(in_catalogue), value = TRUE)) in_catalogue[[n]] <- as.integer(in_catalogue[[n]])
stopifnot(nrow(in_catalogue) == 67, !anyNA(in_catalogue$verdict),
          all.equal(in_catalogue$icc_strict, round(cat0[["photo filter pass"]], 3)))

# ---- segmentation benchmark ------------------------------------------------
det <- rd("table1_detectors.csv")
names(det) <- c("detector", "prompt", "found", "median_score", "sec_detect", "errors")
cmb <- rd("table2_combinations.csv")
cmb <- cmb[, c("detector", "prompt", "segmenter", "n", "mask_ok", "iou_cons", "trait_err",
               "t_det", "t_seg", "t_total")]
names(cmb) <- c("detector", "prompt", "segmenter", "n", "mask_ok", "iou_consensus", "trait_error",
                "sec_detect", "sec_segment", "sec_total")
cmb <- cmb[order(-cmb$iou_consensus), ]
rownames(cmb) <- NULL
rnd <- function(d) { i <- vapply(d, is.double, logical(1)); d[i] <- lapply(d[i], round, 4); d }
fe <- rd("filter_eval_native.csv")
names(fe) <- c("question", "features", "n", "positives", "auc", "auc_sd", "balanced_accuracy")
fe$features <- sub("zero-shot CLIP \\(no training\\)", "zero-shot CLIP (full crop, no training)", fe$features)
used <- c(usable_any = "clip+dino(masked)", top_view = "clip+dino(masked)", upperside_lep = "clip+dino+QC",
          fully_visible = "clip+dino+QC", sharp = "clip+dino+QC")
fe$used <- fe$features == used[fe$question]
wbal <- rd("white_balance.csv")
augm <- rd("augmentation.csv")
stopifnot(nrow(wbal) == 54, nrow(augm) == 99)
in_benchmark <- list(detectors = rnd(det), combinations = rnd(cmb), photo_filter = rnd(fe),
                     white_balance = rnd(wbal), augmentation = rnd(augm))

# ---- validation photo table ------------------------------------------------
pa <- rd("photos_all.csv", colClasses = c(photo_id = "character"))
ps <- rd("photo_set.csv", colClasses = c(photo_id = "character"))
lab <- rd("labels_480.csv", colClasses = c(photo_id = "character"))
rat <- rd("ratings.csv")
rat$photo_id <- lab$photo_id[match(rat$lab_idx, lab$lab_idx)]

lic <- c("publicdomain/zero/1.0" = "CC0", "licenses/by/4.0" = "CC-BY",
         "licenses/by-nc/4.0" = "CC-BY-NC", "licenses/by-sa/4.0" = "CC-BY-SA",
         "licenses/by-nc-sa/4.0" = "CC-BY-NC-SA", "licenses/by-nc-nd/4.0" = "CC-BY-NC-ND")
lic_code <- function(u) {
  out <- rep(NA_character_, length(u))
  for (k in names(lic)) out[grepl(k, u, fixed = TRUE)] <- lic[[k]]
  out
}
m <- match(pa$photo_id, ps$photo_id)
ml <- match(pa$photo_id, lab$photo_id)
mr <- match(pa$photo_id, rat$photo_id)
ext <- setdiff(in_catalogue$trait, core)
in_validation <- data.frame(
  photo_id = pa$photo_id,
  occurrenceKey = as.character(pa$occurrenceKey),
  role = pa$role,
  order = pa$order,
  family = ps$family[m],
  species = pa$species,
  country = ps$country[m],
  year = as.integer(ps$year[m]),
  license = lic_code(pa$license),
  pa[, c("photo_w", "photo_h", "det1_score", "n_other_insects", "cut_by_frame", "det_score",
         "sam_iou", "stab", "iou_stage1", "px", "mask_frac", "border_touch", "mask_ok", "solidity",
         "clip_hi", "clip_lo", "sharp", "insect_px_native",
         "p_usable_any", "p_top_view", "p_upperside_lep", "p_fully_visible", "p_sharp")],
  label_view = lab$view[ml], label_wings = lab$wings[ml], label_visibility = lab$vis[ml],
  label_focus = lab$focus[ml], label_several = as.logical(lab$multi[ml]),
  label_usable = as.logical(lab$usable_any[ml]), label_usable_top = as.logical(lab$usable_top[ml]),
  label_lep_side = ifelse(lab$lep_side[ml] == "na", NA, lab$lep_side[ml]),
  outline_rating = factor(rat$seg_rating[mr], levels = c("G", "M", "B"),
                          labels = c("good", "minor error", "wrong")),
  pa[, c("D_px", core, ext)],
  stringsAsFactors = FALSE, check.names = FALSE)
in_validation$mask_ok <- as.logical(in_validation$mask_ok)
in_validation$cut_by_frame <- as.logical(in_validation$cut_by_frame)
for (n in c("photo_w", "photo_h", "n_other_insects", "px")) in_validation[[n]] <- as.integer(in_validation[[n]])
in_validation$role[in_validation$role == "bench"] <- "benchmark"
stopifnot(nrow(in_validation) == 780, sum(!is.na(in_validation$outline_rating)) == 480,
          sum(!is.na(in_validation$label_view)) == 480, !anyNA(in_validation$license),
          all(in_catalogue$trait %in% names(in_validation)))

dir.create("data", showWarnings = FALSE)
save(in_catalogue, file = file.path("data", "in_catalogue.rda"), compress = "xz")
save(in_benchmark, file = file.path("data", "in_benchmark.rda"), compress = "xz")
save(in_validation, file = file.path("data", "in_validation.rda"), compress = "xz")
