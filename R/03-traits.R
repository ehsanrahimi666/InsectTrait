#' Convert sRGB values to CIELAB
#'
#' Removes the sRGB gamma, then converts linear RGB to CIE XYZ and CIELAB with a
#' D65 white point, using the constants of 'OpenCV' (`COLOR_LRGB2Lab`), so that
#' the values match the 'Python' back end.
#'
#' @param img RGB array (height x width x 3, values 0-255), a file path, or a
#'   three-column matrix of RGB values.
#' @return An array (or matrix) of the same shape with `L*` (0-100), `a*` and `b*`.
#' @examples
#' in_rgb_to_lab(rbind(c(255, 255, 255), c(128, 128, 128), c(200, 30, 30)))
#' @export
in_rgb_to_lab <- function(img) {
  if (is.matrix(img) && ncol(img) == 3L) {
    lab <- .in_lab(.in_lin(img[, 1]), .in_lin(img[, 2]), .in_lin(img[, 3]))
    out <- cbind(L = lab$L, a = lab$a, b = lab$b)
    return(out)
  }
  img <- .in_as_image(img)
  lab <- .in_lab(.in_lin(img[, , 1]), .in_lin(img[, , 2]), .in_lin(img[, , 3]))
  array(c(lab$L, lab$a, lab$b), dim(img))
}

#' Core colour traits and outline quality from a photo and its outline
#'
#' Computes, natively in R, the outline quality metrics and the nine core
#' colour traits of the back end for an insect crop and its outline mask.
#' Formulas and constants follow the 'Python' back end (linear sRGB, 'OpenCV'
#' CIELAB, 'OpenCV' Otsu threshold and Laplacian), so values agree with
#' [in_process()] to within rounding of the image decoder. Edge density and the
#' 57 extended traits need the back end ([in_extended_traits()]).
#'
#' @param img Crop as an RGB array (values 0-255) or a path to a JPEG/PNG
#'   file. A character vector of several paths processes several photos.
#' @param mask Logical outline matrix of the same size, or a path (or paths)
#'   to PNG masks.
#' @param min_px Minimum outline size (pixels) for traits to be computed; the
#'   validated value is 600.
#' @param white_balance Optional colour correction of the photo before the
#'   colour traits are computed (quality metrics use the original photo); see
#'   [in_white_balance()]. The validated default is `"none"`.
#' @return A data frame with one row per photo. Quality columns: `px` (outline
#'   pixels), `mask_frac`, `border_touch` (share of the crop border covered by
#'   the outline), `solidity`, `clip_hi` and `clip_lo` (share of insect pixels
#'   with the brightest channel >= 250 or <= 5), `sharp` (variance of the
#'   Laplacian inside the outline) and `mask_ok` (`px >= min_px` and
#'   `border_touch <= 0.10`; the back end also requires a detection). Trait
#'   columns: `D_px` (equivalent diameter) and the core traits described in
#'   [in_catalogue]. With file paths, a first column `photo_id` holds the file
#'   name without extension.
#' @seealso [in_trait_info()] for definitions and reliability.
#' @examples
#' ex <- in_example_photos()
#' in_traits(ex$crop[1], ex$mask[1])
#'
#' # several photos at once
#' tr <- in_traits(ex$crop, ex$mask)
#' tr[, c("photo_id", "L_mean", "C_mean", "dark_frac_otsu")]
#'
#' # a synthetic two-tone disk: half the insect is dark
#' img <- array(150, c(120, 120, 3))
#' yy <- row(img[, , 1]); xx <- col(img[, , 1])
#' disk <- (yy - 60)^2 + (xx - 60)^2 < 45^2
#' img[, , 1][disk] <- 220; img[, , 2][disk] <- 120; img[, , 3][disk] <- 30
#' img[, , 1][disk & xx < 60] <- 30
#' img[, , 2][disk & xx < 60] <- 25
#' img[, , 3][disk & xx < 60] <- 20
#' in_traits(img, disk)$dark_frac_otsu
#' @export
in_traits <- function(img, mask, min_px = 600,
                      white_balance = c("none", "gray_world", "white_patch", "shades_of_gray", "gray_pixels")) {
  white_balance <- match.arg(white_balance)
  if (is.character(img) && length(img) > 1L) {
    if (!is.character(mask) || length(mask) != length(img)) {
      stop("Give one mask path per image path.", call. = FALSE)
    }
    rows <- Map(function(i, m) in_traits(i, m, min_px = min_px, white_balance = white_balance),
                img, mask)
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    return(out)
  }
  id <- if (is.character(img)) tools::file_path_sans_ext(basename(img)) else NULL
  img <- .in_as_image(img)
  mask <- .in_as_mask(mask)
  if (!identical(dim(img)[1:2], dim(mask))) {
    stop("`img` and `mask` must have the same height and width.", call. = FALSE)
  }
  q <- .in_quality(img, mask, min_px)
  if (white_balance != "none") img <- in_white_balance(img, white_balance)
  out <- c(q, .in_core(img, mask, min_px))
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  if (!is.null(id)) out <- cbind(photo_id = id, out, stringsAsFactors = FALSE)
  out
}

.in_f32 <- function(v) {
  readBin(writeBin(as.double(v), raw(), size = 4), "double", size = 4, n = length(v))
}

# sRGB (0-255) -> linear RGB through a lookup table rounded to float32, as in
# the back end (which works in float32).
.in_lin_lut <- local({
  v <- (0:255) / 255
  .in_f32(ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4))
})

.in_lin <- function(x) {
  i <- as.integer(round(x))
  if (anyNA(i) || any(i < 0L | i > 255L)) stop("RGB values must lie in 0-255.", call. = FALSE)
  out <- .in_lin_lut[i + 1L]
  dim(out) <- dim(x)
  out
}

.in_lab <- function(R, G, B) {
  X <- (0.412453 * R + 0.357580 * G + 0.180423 * B) / 0.950456
  Y <- 0.212671 * R + 0.715160 * G + 0.072169 * B
  Z <- (0.019334 * R + 0.119193 * G + 0.950227 * B) / 1.088754
  f <- function(t) {
    o <- 7.787 * t + 16 / 116
    i <- t > 0.008856
    o[i] <- t[i]^(1 / 3)
    o
  }
  fX <- f(X)
  fY <- f(Y)
  fZ <- f(Z)
  L <- 903.3 * Y
  i <- Y > 0.008856
  L[i] <- 116 * fY[i] - 16
  list(L = L, a = 500 * (fX - fY), b = 200 * (fY - fZ))
}

# Erosion with a square (2k + 1) element; pixels outside the image count as
# insect, as in OpenCV's default border for erosion.
.in_erode <- function(m, k) {
  h <- nrow(m)
  w <- ncol(m)
  p <- cbind(matrix(TRUE, h, k), m, matrix(TRUE, h, k))
  r <- m
  for (d in 0:(2 * k)) r <- r & p[, (1 + d):(w + d), drop = FALSE]
  p <- rbind(matrix(TRUE, k, w), r, matrix(TRUE, k, w))
  o <- r
  for (d in 0:(2 * k)) o <- o & p[(1 + d):(h + d), , drop = FALSE]
  o
}

# Index vector for reflect-101 padding by one pixel.
.in_r101 <- function(n) if (n > 1L) c(2L, seq_len(n), n - 1L) else c(1L, 1L, 1L)

# OpenCV's Otsu threshold for an 8-bit vector (getThreshVal_Otsu_8u).
.in_otsu <- function(v) {
  h <- tabulate(v + 1L, nbins = 256L)
  N <- length(v)
  sc <- 1 / N
  mu <- sum((0:255) * h) * sc
  mu1 <- 0
  q1 <- 0
  max_sigma <- 0
  max_val <- 0
  eps <- 1.192092896e-07
  for (i in 0:255) {
    p_i <- h[i + 1L] * sc
    mu1 <- mu1 * q1
    q1 <- q1 + p_i
    q2 <- 1 - q1
    if (min(q1, q2) < eps || max(q1, q2) > 1 - eps) next
    mu1 <- (mu1 + i * p_i) / q1
    mu2 <- (mu - q1 * mu1) / q2
    sigma <- q1 * q2 * (mu1 - mu2)^2
    if (sigma > max_sigma) {
      max_sigma <- sigma
      max_val <- i
    }
  }
  max_val
}

.in_quality <- function(img, mask, min_px) {
  h <- nrow(mask)
  w <- ncol(mask)
  px <- sum(mask)
  na <- NA_real_
  q <- list(px = as.integer(px), mask_frac = px / (h * w), border_touch = na,
            solidity = na, clip_hi = na, clip_lo = na, sharp = na, mask_ok = FALSE)
  if (px == 0) return(q)
  q$border_touch <- mean(c(mask[1, ], mask[h, ], mask[, 1], mask[, w]))
  idx <- which(mask)
  ys <- (idx - 1L) %% h + 1L
  xs <- (idx - 1L) %/% h + 1L
  first <- !duplicated(ys)
  last <- !duplicated(ys, fromLast = TRUE)
  hx <- c(xs[first], xs[last])
  hy <- c(ys[first], ys[last])
  ch <- grDevices::chull(hx, hy)
  hull <- abs(sum(hx[ch] * c(hy[ch][-1], hy[ch][1]) - c(hx[ch][-1], hx[ch][1]) * hy[ch])) / 2
  q$solidity <- if (hull > 0) px / hull else na
  mx <- pmax(img[, , 1][idx], img[, , 2][idx], img[, , 3][idx])
  q$clip_hi <- mean(mx >= 250)
  q$clip_lo <- mean(mx <= 5)
  gray <- (img[, , 1] * 4899 + img[, , 2] * 9617 + img[, , 3] * 1868 + 8192) %/% 16384
  g <- gray[.in_r101(h), .in_r101(w), drop = FALSE]
  lap <- g[1:h, 2:(w + 1), drop = FALSE] + g[3:(h + 2), 2:(w + 1), drop = FALSE] +
    g[2:(h + 1), 1:w, drop = FALSE] + g[2:(h + 1), 3:(w + 2), drop = FALSE] - 4 * gray
  inner <- .in_erode(mask, 2L)
  if (sum(inner) > 50) {
    v <- lap[inner]
    q$sharp <- mean((v - mean(v))^2)
  }
  q$mask_ok <- px >= min_px && q$border_touch <= 0.10
  q
}

.in_core <- function(img, mask, min_px) {
  out <- stats::setNames(as.list(rep(NA_real_, 10L)), c("D_px", .in_core_traits))
  px <- sum(mask)
  if (px < min_px) return(out)
  h <- nrow(mask)
  R <- .in_lin(img[, , 1])
  G <- .in_lin(img[, , 2])
  B <- .in_lin(img[, , 3])
  Yall <- 0.2126 * R + 0.7152 * G + 0.0722 * B
  y99 <- stats::quantile(Yall, 0.99, names = FALSE, type = 7) + 1e-6
  idx <- which(mask)
  lab <- .in_lab(R[idx], G[idx], B[idx])
  L <- .in_f32(lab$L)
  a <- lab$a
  b <- lab$b
  C <- sqrt(a^2 + b^2)
  hx <- sum(a) / (sum(C) + 1e-6)
  hy <- sum(b) / (sum(C) + 1e-6)
  D <- 2 * sqrt(px / pi)
  ys <- (idx - 1L) %% h + 1L
  xs <- (idx - 1L) %/% h + 1L
  wt <- exp(-sqrt((ys - mean(ys))^2 + (xs - mean(xs))^2) / D / 0.25)
  Y <- Yall[idx]
  L8 <- floor(pmin(pmax(.in_f32(L * .in_f32(2.55)), 0), 255))
  thr <- .in_otsu(as.integer(L8))
  Lin <- L[L >= 0 & L <= 100]
  bins <- pmin(floor(Lin / 2), 49)
  p <- tabulate(bins + 1L, nbins = 50L)
  p <- p[p > 0] / sum(p)
  out$D_px <- round(D, 1)
  out$L_mean <- mean(L)
  out$L_sd <- sqrt(mean((L - mean(L))^2))
  out$C_mean <- mean(C)
  out$hue_deg <- (atan2(hy, hx) * 180 / pi) %% 360
  out$hue_conc <- sqrt(hx^2 + hy^2)
  out$vap_wp <- 1 - min(1, mean(Y) / y99)
  out$darkness_cw <- sum((1 - Y) * wt) / sum(wt)
  out$dark_frac_otsu <- mean(L8 <= thr)
  out$L_entropy <- -sum(p * log2(p))
  out
}
