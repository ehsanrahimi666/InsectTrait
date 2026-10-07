#' Plot a photo with the insect outline
#'
#' @param img RGB array (values 0-255) or file path.
#' @param mask Outline matrix or file path; `NULL` plots the photo only.
#' @param col,lwd Colour and width of the outline.
#' @param main Optional title.
#' @return `NULL`, invisibly.
#' @examples
#' ex <- in_example_photos()
#' in_plot_outline(ex$crop[1], ex$mask[1], main = ex$species[1])
#' @export
in_plot_outline <- function(img, mask = NULL, col = "#FFD400", lwd = 2, main = NULL) {
  .in_panel(.in_as_image(img), if (is.null(mask)) NULL else .in_as_mask(mask), col, lwd,
            main, font = 3)
}

.in_panel <- function(img, mask, col, lwd, main, font = 1, cex = 0.95) {
  h <- dim(img)[1]
  w <- dim(img)[2]
  op <- graphics::par(mar = c(0.2, 0.2, if (is.null(main)) 0.2 else 1.6, 0.2))
  on.exit(graphics::par(op))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, w), ylim = c(0, h), asp = 1, xaxs = "i", yaxs = "i")
  graphics::rasterImage(grDevices::as.raster(img / 255), 0, 0, w, h, interpolate = FALSE)
  if (!is.null(mask)) .in_draw_outline(mask, col, lwd)
  if (!is.null(main)) graphics::title(main, cex.main = cex, font.main = font, line = 0.4)
  invisible(NULL)
}

.in_draw_outline <- function(m, col, lwd) {
  h <- nrow(m)
  w <- ncol(m)
  z <- matrix(0, w + 2, h + 2)
  z[2:(w + 1), 2:(h + 1)] <- t(m[h:1, , drop = FALSE])
  cl <- grDevices::contourLines(x = (0:(w + 1)) - 0.5, y = (0:(h + 1)) - 0.5, z = z, levels = 0.5)
  for (k in cl) graphics::lines(k$x, k$y, col = col, lwd = lwd)
}

#' Contact sheet of crops and outlines
#'
#' Draws a grid of insect crops with their outlines, green for kept and red for
#' rejected photos, labelled with the decision. Use it to check a run by eye.
#'
#' @param x A `in_results` data frame.
#' @param dir Results folder with `crops/` and `masks/`; defaults to
#'   `attr(x, "dir")`.
#' @param n Maximum number of photos (a random sample if `x` has more rows).
#' @param ncol Number of columns.
#' @param label Column used as the label under each crop.
#' @param file Optional PNG file to write instead of the current device.
#' @param seed Random seed for the sample.
#' @return The rows of `x` that were drawn, invisibly.
#' @examples
#' res <- in_example_results()
#' in_contact_sheet(res, ncol = 4)
#' @export
in_contact_sheet <- function(x, dir = attr(x, "dir"), n = 24, ncol = 6, label = "reason",
                             file = NULL, seed = 1) {
  if (is.null(dir)) stop("Give the results folder in `dir`.", call. = FALSE)
  crop <- file.path(dir, "crops", paste0(x$photo_id, ".jpg"))
  mask <- file.path(dir, "masks", paste0(x$photo_id, ".png"))
  ok <- file.exists(crop) & file.exists(mask)
  x <- x[ok, , drop = FALSE]
  crop <- crop[ok]
  mask <- mask[ok]
  if (!nrow(x)) stop("No crops and masks found in ", dir, call. = FALSE)
  if (nrow(x) > n) {
    i <- sort(.in_with_seed(seed, sample(nrow(x), n)))
    x <- x[i, , drop = FALSE]
    crop <- crop[i]
    mask <- mask[i]
  }
  ncol <- min(ncol, nrow(x))
  nrow_ <- ceiling(nrow(x) / ncol)
  if (!is.null(file)) {
    grDevices::png(file, width = 300 * ncol, height = 330 * nrow_, res = 120)
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  op <- graphics::par(mfrow = c(nrow_, ncol), oma = c(0, 0, 0, 0))
  on.exit(graphics::par(op), add = TRUE)
  keep <- if ("keep" %in% names(x)) x$keep else rep(TRUE, nrow(x))
  lab <- if (label %in% names(x)) as.character(x[[label]]) else x$photo_id
  for (i in seq_len(nrow(x))) {
    .in_panel(.in_shrink(in_read_image(crop[i]), 400), .in_mask_fit(mask[i], 400),
              col = if (isTRUE(keep[i])) "#00C853" else "#FF1744", lwd = 1.6, main = lab[i],
              font = if (label == "species") 3 else 1, cex = 0.7)
  }
  invisible(x)
}

.in_mask_fit <- function(path, max_side) {
  m <- in_read_mask(path)
  s <- max_side / max(dim(m))
  if (s >= 1) return(m)
  .in_area_resize(m * 1, max(1L, round(nrow(m) * s)), max(1L, round(ncol(m) * s))) > 0.5
}

#' Plot a trait by group
#'
#' Box plots with the individual photos overlaid, for kept photos only by
#' default.
#'
#' @param x Data frame of traits, for example from [in_read_results()].
#' @param trait Trait column.
#' @param group Grouping column.
#' @param keep_only Use only rows with `keep == TRUE`.
#' @param ... Passed to [graphics::boxplot()].
#' @return The data used, invisibly.
#' @examples
#' v <- in_filter(in_validation, "default")
#' v <- v[v$order == "Coleoptera", ]
#' in_plot_traits(v, "dark_frac_otsu", group = "species")
#' @export
in_plot_traits <- function(x, trait, group = "species", keep_only = TRUE, ...) {
  if (!all(c(trait, group) %in% names(x))) stop("Columns not found.", call. = FALSE)
  if (keep_only && "keep" %in% names(x)) x <- x[x$keep %in% TRUE, , drop = FALSE]
  d <- data.frame(y = as.numeric(x[[trait]]), g = factor(x[[group]]))
  d <- d[is.finite(d$y), , drop = FALSE]
  d$g <- droplevels(d$g)
  op <- graphics::par(mar = c(4, max(6, 0.55 * max(nchar(levels(d$g)), 1)), 1, 1))
  on.exit(graphics::par(op))
  graphics::boxplot(y ~ g, data = d, horizontal = TRUE, las = 1, outline = FALSE,
                    col = "#DCE9F5", border = "#4A6FA5", xlab = trait, ylab = "",
                    cex.axis = 0.75, ...)
  graphics::stripchart(y ~ g, data = d, method = "jitter", jitter = 0.15, add = TRUE,
                       pch = 16, cex = 0.5, col = grDevices::adjustcolor("#1F3B66", 0.6))
  invisible(d)
}

#' Plot repeatability estimates
#'
#' @param r Output of [in_repeatability()].
#' @param threshold Reference line (0.6 is the catalogue threshold for a
#'   reliable trait).
#' @return `r`, invisibly.
#' @examples
#' v <- in_filter(in_validation, "default")
#' r <- in_repeatability(v[v$keep, ], traits = c("L_mean", "L_sd", "C_mean",
#'                       "hue_conc", "dark_frac_otsu"), boot = 50)
#' in_plot_repeatability(r)
#' @export
in_plot_repeatability <- function(r, threshold = 0.6) {
  r <- r[order(r$icc), , drop = FALSE]
  n <- nrow(r)
  op <- graphics::par(mar = c(4, 9, 1, 1))
  on.exit(graphics::par(op))
  graphics::plot(r$icc, seq_len(n), xlim = c(min(0, r$lower, na.rm = TRUE), 1), yaxt = "n",
                 pch = 19, col = "#1F3B66", xlab = "Photo-to-photo ICC", ylab = "")
  graphics::segments(r$lower, seq_len(n), r$upper, seq_len(n), col = "#1F3B66")
  graphics::axis(2, at = seq_len(n), labels = r$trait, las = 1, cex.axis = 0.8)
  graphics::abline(v = threshold, lty = 2, col = "grey50")
  invisible(r)
}
