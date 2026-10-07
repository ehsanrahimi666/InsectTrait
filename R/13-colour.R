#' Colour correction (white balance)
#'
#' Estimates the colour of the light from the photo and divides it out in linear
#' RGB, as in the back end: grey world (mean of all pixels), white patch (99th
#' percentile), shades of grey (Minkowski mean with `p = 6`) or grey pixels
#' (mean of near-neutral pixels: saturation below 0.2 and brightest channel
#' between 10% and 98%). The estimate is scaled to keep the overall
#' brightness. Grey world assumes that the scene is grey on average, which green
#' vegetation and coloured flowers violate; grey pixels uses only pixels that are
#' probably neutral and leaves the photo unchanged when fewer than 1% of the
#' pixels qualify. Check the effect before use (see [in_colour_check()]).
#'
#' @param img RGB array (values 0-255) or file path.
#' @param method `"gray_world"`, `"white_patch"`, `"shades_of_gray"` or
#'   `"gray_pixels"`.
#' @param mask Optional outline; with `use = "background"`, the light is
#'   estimated from pixels outside the insect.
#' @param use `"all"` pixels or `"background"` pixels.
#' @param p Exponent for shades of grey.
#' @return The corrected RGB array (values 0-255).
#' @examples
#' ex <- in_example_photos()
#' img <- in_read_image(ex$crop[2])
#' wb <- in_white_balance(img, "gray_world")
#' op <- par(mfrow = c(1, 2)); in_plot_outline(img); in_plot_outline(wb); par(op)
#' @export
in_white_balance <- function(img, method = c("gray_world", "white_patch", "shades_of_gray",
                                              "gray_pixels"),
                             mask = NULL, use = c("all", "background"), p = 6) {
  method <- match.arg(method)
  use <- match.arg(use)
  img <- .in_as_image(img)
  lin <- lapply(1:3, function(ch) .in_lin(img[, , ch]))
  sel <- if (use == "background" && !is.null(mask)) !.in_as_mask(mask) else
    matrix(TRUE, dim(img)[1], dim(img)[2])
  if (method == "gray_pixels") {
    sel <- sel & .in_neutral(img)
    if (sum(sel) < max(50, 0.01 * length(sel))) return(img)
    method <- "gray_world"
  }
  e <- vapply(lin, function(L) {
    v <- L[sel]
    switch(method, gray_world = mean(v), white_patch = stats::quantile(v, 0.99, names = FALSE),
           shades_of_gray = mean(v^p)^(1 / p))
  }, numeric(1))
  e <- pmax(e, 1e-6)
  e <- 3 * e / sum(e)
  out <- img
  for (ch in 1:3) out[, , ch] <- .in_to_srgb8(lin[[ch]] / e[ch])
  out
}

#' Check the colour and exposure of photos
#'
#' Screens photos for a colour cast, clipped highlights and under-exposure. The
#' colour cast is estimated from near-neutral pixels outside the insect (see
#' `"gray_pixels"` in [in_white_balance()]) and expressed as log ratios of red
#' and blue to green, and as the spread `cast` = log(largest / smallest channel);
#' it is `NA` when fewer than 1% of the pixels are neutral.
#' The grey-world estimate (`gw_rg`, `gw_bg`) is reported for comparison; it is
#' dominated by the colour of the background in most field photos. The
#' thresholds are screening defaults, not validated limits.
#'
#' @param img RGB array, file path, or several file paths.
#' @param mask Optional outline (or paths, one per image).
#' @param max_cast Largest acceptable spread of the light estimate between
#'   channels, as a log ratio (0.2 is about a 22% imbalance).
#' @param max_clip Largest acceptable share of clipped pixels (brightest
#'   channel of at least 250) in the insect, or in the photo without a mask.
#' @param max_dark Largest acceptable share of very dark pixels (brightest
#'   channel of at most 5).
#' @return A data frame with `neutral` (share of near-neutral pixels),
#'   `cast_rg`, `cast_bg`, `cast`, `gw_rg`, `gw_bg`, `clip`, `dark` and `issues`
#'   (`"ok"` or a list of problems), with `photo_id` for file input.
#' @examples
#' ex <- in_example_photos()
#' in_colour_check(ex$crop, ex$mask)
#' @export
in_colour_check <- function(img, mask = NULL, max_cast = 0.2, max_clip = 0.02, max_dark = 0.05) {
  if (is.character(img) && length(img) > 1L) {
    if (!is.null(mask) && length(mask) != length(img)) stop("Give one mask per image.", call. = FALSE)
    rows <- lapply(seq_along(img), function(i) in_colour_check(img[i], if (is.null(mask)) NULL else mask[i],
                                                               max_cast, max_clip, max_dark))
    return(do.call(rbind, rows))
  }
  id <- if (is.character(img)) tools::file_path_sans_ext(basename(img)) else NULL
  img <- .in_as_image(img)
  m <- if (is.null(mask)) NULL else .in_as_mask(mask)
  bg <- if (is.null(m)) matrix(TRUE, dim(img)[1], dim(img)[2]) else !m
  lin <- lapply(1:3, function(ch) .in_lin(img[, , ch]))
  gw <- pmax(vapply(lin, function(L) mean(L[bg]), numeric(1)), 1e-6)
  neu <- bg & .in_neutral(img)
  ok <- sum(neu) >= max(50, 0.01 * length(neu))
  e <- if (ok) pmax(vapply(lin, function(L) mean(L[neu]), numeric(1)), 1e-6) else rep(NA_real_, 3)
  mx <- pmax(img[, , 1], img[, , 2], img[, , 3])
  sel <- if (is.null(m)) TRUE else m
  out <- data.frame(neutral = mean(neu), cast_rg = log(e[1] / e[2]), cast_bg = log(e[3] / e[2]))
  out$cast <- log(max(e) / min(e))
  out$gw_rg <- log(gw[1] / gw[2])
  out$gw_bg <- log(gw[3] / gw[2])
  out$clip <- mean(mx[sel] >= 250)
  out$dark <- mean(mx[sel] <= 5)
  issues <- c(if (!is.na(out$cast) && out$cast > max_cast) "colour cast",
              if (out$clip > max_clip) "clipped highlights", if (out$dark > max_dark) "under-exposed")
  out$issues <- if (length(issues)) paste(issues, collapse = "; ") else "ok"
  if (!is.null(id)) out <- cbind(photo_id = id, out, stringsAsFactors = FALSE)
  out
}

#' Check trait stability under augmentation
#'
#' Applies augmentations of the kind used to train image models (rotations,
#' mirroring, colour jitter) and camera changes ([in_augment()]) to a photo,
#' recomputes the core traits and reports how much each trait moves. Changes are
#' expressed relative to the between-photo standard deviation of the trait in
#' the validation photos kept by the default rule, so that a value of 0.25 means
#' a quarter of the usual difference between photos. Rotations and mirroring do
#' not change colour statistics apart from resampling; colour jitter shows how
#' sensitive a trait is to camera processing.
#'
#' @param img RGB array or file path; several paths process several photos.
#' @param mask Outline matrix or file path (one per image).
#' @param types Augmentations to apply; see [in_augment()].
#' @param n Random draws per augmentation.
#' @param tol Relative change above which a trait is flagged (a screening
#'   default, not a validated limit).
#' @param seed Random seed.
#' @return A data frame with `type`, `trait`, `original`, `mean_change`
#'   (mean absolute change), `rel_change` (largest absolute change divided by
#'   the between-photo standard deviation) and `flag`, with `photo_id` for file
#'   input.
#' @examples
#' ex <- in_example_photos()
#' chk <- in_augment_check(ex$crop[1], ex$mask[1], types = c("rotate90", "hue", "saturation"))
#' chk[chk$trait %in% c("L_mean", "C_mean", "hue_deg"), ]
#' @export
in_augment_check <- function(img, mask, types = c("rotate90", "flip", "rotate", "brightness",
                                                  "contrast", "saturation", "hue", "white_balance",
                                                  "exposure", "jpeg", "blur"),
                             n = 1, tol = 0.25, seed = 1) {
  if (is.character(img) && length(img) > 1L) {
    if (length(mask) != length(img)) stop("Give one mask per image.", call. = FALSE)
    rows <- lapply(seq_along(img), function(i) in_augment_check(img[i], mask[i], types, n, tol, seed + i))
    return(do.call(rbind, rows))
  }
  id <- if (is.character(img)) tools::file_path_sans_ext(basename(img)) else NULL
  img <- .in_as_image(img)
  mask <- .in_as_mask(mask)
  tr <- .in_core_traits
  base <- unlist(in_traits(img, mask)[tr])
  ref <- .in_ref_sd()
  rows <- .in_with_seed(seed, lapply(types, function(tp) {
    d <- t(vapply(seq_len(n), function(k) {
      a <- in_augment(img, mask, tp)
      v <- unlist(in_traits(a$image, a$mask)[tr])
      dv <- abs(v - base)
      dv["hue_deg"] <- pmin(dv["hue_deg"], 360 - dv["hue_deg"])
      dv
    }, numeric(length(tr))))
    data.frame(type = tp, trait = tr, original = unname(base), mean_change = colMeans(d),
               rel_change = apply(d, 2, max) / ref[tr], stringsAsFactors = FALSE)
  }))
  out <- do.call(rbind, rows)
  out$flag <- !is.na(out$rel_change) & out$rel_change > tol
  rownames(out) <- NULL
  if (!is.null(id)) out <- cbind(photo_id = id, out, stringsAsFactors = FALSE)
  out
}

# Near-neutral pixels: low saturation, neither dark nor clipped (sRGB).
.in_neutral <- function(img) {
  mx <- pmax(img[, , 1], img[, , 2], img[, , 3]) / 255
  mn <- pmin(img[, , 1], img[, , 2], img[, , 3]) / 255
  s <- ifelse(mx > 0, (mx - mn) / mx, 0)
  s < 0.2 & mx > 0.1 & mx < 0.98
}

# Between-photo SD of the core traits in validation photos kept by the default rule.
.in_ref_sd <- function() {
  v <- in_filter(in_validation, "default")
  v <- v[v$keep, ]
  sd <- vapply(.in_core_traits, function(t) stats::sd(v[[t]], na.rm = TRUE), numeric(1))
  r <- v$hue_deg[is.finite(v$hue_deg)] * pi / 180
  sd["hue_deg"] <- sqrt(-2 * log(sqrt(mean(sin(r))^2 + mean(cos(r))^2))) * 180 / pi
  sd
}
