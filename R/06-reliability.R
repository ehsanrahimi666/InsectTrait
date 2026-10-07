#' Intraclass correlation ICC(1)
#'
#' One-way random-effects intraclass correlation for unbalanced groups
#' (Shrout and Fleiss ICC(1), with the average group size `k0` of the
#' unbalanced one-way ANOVA). Missing and infinite values and groups with fewer
#' than two values are dropped.
#'
#' @param values Numeric vector.
#' @param groups Grouping vector of the same length (for example the occurrence
#'   key, so that photos of the same insect form a group).
#' @return The ICC, or `NA` with fewer than three groups or no variance.
#' @examples
#' set.seed(1)
#' insect <- rep(1:30, each = 2)
#' true <- rnorm(30)[insect]
#' in_icc(true + rnorm(60, sd = 0.3), insect)  # repeatable trait
#' in_icc(rnorm(60), insect)                   # pure noise, near 0
#' @export
in_icc <- function(values, groups) {
  ok <- is.finite(values) & !is.na(groups)
  v <- values[ok]
  g <- as.character(groups[ok])
  keep <- g %in% names(which(table(g) >= 2L))
  v <- v[keep]
  g <- g[keep]
  if (length(unique(g)) < 3L || stats::var(v) == 0) return(NA_real_)
  k <- tapply(v, g, length)
  m <- tapply(v, g, mean)
  N <- length(v)
  a <- length(k)
  msb <- sum(k * (m - mean(v))^2) / (a - 1)
  msw <- sum((v - m[g])^2) / (N - a)
  k0 <- (N - sum(k^2) / N) / (a - 1)
  den <- msb + (k0 - 1) * msw
  if (den > 0) (msb - msw) / den else NA_real_
}

#' Photo-to-photo repeatability of traits
#'
#' Estimates, for each trait, the ICC(1) between photos of the same insect
#' (rows sharing `group`), with a bootstrap confidence interval obtained by
#' resampling insects. Circular traits (hue angles) are rotated so that their
#' circular mean lies at 180 degrees before the ICC is computed. This is the
#' repeatability reported in [in_catalogue].
#'
#' @param x Data frame of traits with a grouping column, for example kept rows
#'   of [in_read_results()] or [in_validation].
#' @param traits Trait columns. By default, all catalogue traits found in `x`.
#' @param group Column that identifies the insect (several photos per group).
#' @param boot Number of bootstrap resamples (0 for none).
#' @param conf Confidence level of the interval.
#' @param seed Random seed for the bootstrap.
#' @return A data frame with `trait`, `icc`, `lower`, `upper`, `n_groups` and
#'   `n_photos`.
#' @examples
#' v <- in_filter(in_validation, "default")
#' rep <- in_repeatability(v[v$keep, ], traits = c("L_mean", "C_mean", "hue_deg"),
#'                         boot = 50)
#' rep
#' @export
in_repeatability <- function(x, traits = NULL, group = "occurrenceKey", boot = 200,
                             conf = 0.95, seed = 1) {
  if (!group %in% names(x)) stop("Column `", group, "` not found.", call. = FALSE)
  if (is.null(traits)) traits <- intersect(in_catalogue$trait, names(x))
  miss <- setdiff(traits, names(x))
  if (length(miss)) stop("Traits not found: ", paste(miss, collapse = ", "), call. = FALSE)
  alpha <- (1 - conf) / 2
  rows <- lapply(traits, function(t) {
    v <- suppressWarnings(as.numeric(x[[t]]))
    if (t %in% .in_circular) v <- .in_rotate(v)
    g <- as.character(x[[group]])
    ok <- is.finite(v) & !is.na(g)
    v <- v[ok]
    g <- g[ok]
    multi <- g %in% names(which(table(g) >= 2L))
    v <- v[multi]
    g <- g[multi]
    est <- in_icc(v, g)
    lo <- hi <- NA_real_
    if (boot > 0 && !is.na(est)) {
      sp <- split(v, g)
      bs <- .in_with_seed(seed, vapply(seq_len(boot), function(b) {
        s <- sample(length(sp), replace = TRUE)
        vals <- unlist(sp[s], use.names = FALSE)
        in_icc(vals, rep(seq_along(s), lengths(sp[s])))
      }, numeric(1)))
      q <- stats::quantile(bs, c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE, type = 7)
      lo <- q[1]
      hi <- q[2]
    }
    data.frame(trait = t, icc = est, lower = lo, upper = hi,
               n_groups = length(unique(g)), n_photos = length(v), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Augment a photo: camera changes and training-style transformations
#'
#' Changes a photo (and, for geometric changes, its outline) in the ways used
#' to augment training images in computer vision and to test trait invariance.
#' Validated changes (used for [in_catalogue]): white balance (red and blue
#' gains in linear RGB, 0.75-1.25), exposure (linear gain 0.6-1.5), JPEG
#' re-compression (quality 30-89), Gaussian blur (sigma 0.5-1.5 px), rotation
#' (-30 to 30 degrees) and perspective (corners moved by up to 12% of the
#' shorter side). Training-style changes: rotation by 90, 180 or 270 degrees
#' (`"rotate90"`), mirroring (`"flip"`), and colour jitter of brightness,
#' contrast and saturation (factor 0.75-1.25) and hue (shift of up to 0.05 of the
#' hue circle), defined as in common augmentation libraries. Random values are
#' drawn unless `value` is given.
#'
#' @param img RGB array (values 0-255) or file path.
#' @param mask Outline matrix or file path.
#' @param type Type of change; see Description.
#' @param value Optional fixed value: two gains (red, blue) for white balance;
#'   a factor for exposure, brightness, contrast or saturation; a quality
#'   (1-100) for JPEG; a sigma for blur; an angle in degrees for rotation; 1, 2
#'   or 3 quarter turns for `"rotate90"`; `"horizontal"` or `"vertical"` for
#'   `"flip"`; a shift (-0.5 to 0.5) for hue; or a 4 x 2 matrix of corner shifts
#'   (pixels) for perspective.
#' @return A list with `image` and `mask`.
#' @seealso [in_augment_check()], [in_invariance()]
#' @examples
#' ex <- in_example_photos()
#' img <- in_read_image(ex$crop[1]); m <- in_read_mask(ex$mask[1])
#' warm <- in_augment(img, m, "white_balance", value = c(1.2, 0.8))
#' turned <- in_augment(img, m, "rotate90", value = 1)
#' rbind(original = in_traits(img, m)[, c("L_mean", "C_mean", "hue_deg")],
#'       warmer = in_traits(warm$image, warm$mask)[, c("L_mean", "C_mean", "hue_deg")],
#'       rotated = in_traits(turned$image, turned$mask)[, c("L_mean", "C_mean", "hue_deg")])
#' @export
in_augment <- function(img, mask, type = c("white_balance", "exposure", "jpeg", "blur", "rotate",
                                           "perspective", "rotate90", "flip", "brightness",
                                           "contrast", "saturation", "hue"), value = NULL) {
  type <- match.arg(type)
  img <- .in_as_image(img)
  mask <- .in_as_mask(mask)
  h <- nrow(mask)
  w <- ncol(mask)
  if (type %in% c("white_balance", "exposure")) {
    f <- if (type == "white_balance") {
      v <- value %||% stats::runif(2, 0.75, 1.25)
      c(v[1], 1, v[2])
    } else {
      rep(value %||% stats::runif(1, 0.6, 1.5), 3)
    }
    out <- img
    for (ch in 1:3) out[, , ch] <- .in_to_srgb8(.in_lin(img[, , ch]) * f[ch])
    return(list(image = out, mask = mask))
  }
  if (type == "jpeg") {
    q <- value %||% sample(30:89, 1)
    raw <- jpeg::writeJPEG(img / 255, target = raw(), quality = q / 100)
    return(list(image = round(jpeg::readJPEG(raw) * 255), mask = mask))
  }
  if (type == "blur") {
    s <- value %||% stats::runif(1, 0.5, 1.5)
    out <- img
    for (ch in 1:3) out[, , ch] <- round(.in_gauss(img[, , ch], s))
    return(list(image = out, mask = mask))
  }
  if (type == "rotate90") {
    k <- as.integer(value %||% sample(1:3, 1)) %% 4L
    for (i in seq_len(k)) {
      img <- .in_rot90(img)
      mask <- .in_rot90(mask)
    }
    return(list(image = img, mask = mask))
  }
  if (type == "flip") {
    dir <- value %||% sample(c("horizontal", "vertical"), 1)
    if (dir == "horizontal") return(list(image = img[, w:1, , drop = FALSE], mask = mask[, w:1, drop = FALSE]))
    return(list(image = img[h:1, , , drop = FALSE], mask = mask[h:1, , drop = FALSE]))
  }
  if (type %in% c("brightness", "contrast", "saturation")) {
    f <- value %||% stats::runif(1, 0.75, 1.25)
    gray <- 0.2989 * img[, , 1] + 0.587 * img[, , 2] + 0.114 * img[, , 3]
    base <- switch(type, brightness = 0 * gray, contrast = 0 * gray + mean(gray), saturation = gray)
    out <- img
    for (ch in 1:3) out[, , ch] <- round(pmin(pmax(base + f * (img[, , ch] - base), 0), 255))
    return(list(image = out, mask = mask))
  }
  if (type == "hue") {
    shift <- value %||% stats::runif(1, -0.05, 0.05)
    return(list(image = .in_hue_shift(img, shift), mask = mask))
  }
  if (type == "rotate") {
    ang <- (value %||% stats::runif(1, -30, 30)) * pi / 180
    al <- cos(ang)
    be <- sin(ang)
    cx <- w / 2
    cy <- h / 2
    H <- rbind(c(al, be, (1 - al) * cx - be * cy), c(-be, al, be * cx + (1 - al) * cy), c(0, 0, 1))
  } else {
    src <- rbind(c(0, 0), c(w, 0), c(w, h), c(0, h))
    j <- 0.12 * min(w, h)
    sh <- value %||% matrix(stats::runif(8, -j, j), 4, 2)
    H <- .in_homography(src, src + sh)
  }
  .in_warp(img, mask, H)
}

# Quarter turn (clockwise) of a matrix or an h x w x 3 array.
.in_rot90 <- function(x) {
  if (length(dim(x)) == 2L) return(t(x[nrow(x):1, , drop = FALSE]))
  out <- array(0, c(dim(x)[2], dim(x)[1], 3L))
  for (ch in 1:3) out[, , ch] <- t(x[dim(x)[1]:1, , ch, drop = TRUE])
  out
}

# Hue shift through HSV, as in common colour-jitter augmentation.
.in_hue_shift <- function(img, shift) {
  r <- img[, , 1] / 255
  g <- img[, , 2] / 255
  b <- img[, , 3] / 255
  mx <- pmax(r, g, b)
  mn <- pmin(r, g, b)
  d <- mx - mn
  hh <- numeric(length(r))
  i <- d > 0 & mx == r
  hh[i] <- ((g[i] - b[i]) / d[i]) %% 6
  i2 <- d > 0 & mx == g & !i
  hh[i2] <- (b[i2] - r[i2]) / d[i2] + 2
  i3 <- d > 0 & mx == b & !i & !i2
  hh[i3] <- (r[i3] - g[i3]) / d[i3] + 4
  hh <- (hh / 6 + shift) %% 1
  s <- ifelse(mx > 0, d / mx, 0)
  v <- mx
  hp <- hh * 6
  c <- v * s
  x <- c * (1 - abs(hp %% 2 - 1))
  m <- v - c
  seg <- floor(hp) %% 6
  rr <- ifelse(seg %in% c(0, 5), c, ifelse(seg %in% c(1, 4), x, 0))
  gg <- ifelse(seg %in% c(1, 2), c, ifelse(seg %in% c(0, 3), x, 0))
  bb <- ifelse(seg %in% c(3, 4), c, ifelse(seg %in% c(2, 5), x, 0))
  out <- img
  out[, , 1] <- round(255 * (rr + m))
  out[, , 2] <- round(255 * (gg + m))
  out[, , 3] <- round(255 * (bb + m))
  out
}

#' Trait invariance to camera changes and image resolution
#'
#' For each photo, computes the core traits on the original crop, on each
#' simulated change ([in_augment()]) and on copies reduced so that the insect is
#' `rungs` pixels across, with the outline fixed. The ICC(1) between the
#' original and each changed version, across photos, measures how invariant
#' each trait is. In the validation (100 filtered photos), traits were called
#' reliable when photometric invariance was at least 0.8 and the ICC was at
#' least 0.8 with the insect 128 px across.
#'
#' @param images,masks Character vectors of file paths (or lists of arrays and
#'   matrices) of crops and outlines.
#' @param types Changes to apply; see [in_augment()].
#' @param rungs Insect diameters (pixels) for the resolution ladder.
#' @param seed Random seed.
#' @param min_n Minimum number of photos for an ICC.
#' @return A data frame of ICCs, one row per trait and one column per change;
#'   the per-photo traits are in `attr(result, "data")`.
#' @examples
#' ex <- in_example_photos()
#' inv <- in_invariance(ex$crop, ex$mask, types = c("exposure", "blur"),
#'                      rungs = 64, min_n = 3)
#' round(inv, 2)
#' @export
in_invariance <- function(images, masks, types = c("white_balance", "exposure", "jpeg", "blur",
                                                   "rotate", "perspective"),
                          rungs = c(32, 64, 128, 256), seed = 1, min_n = 10) {
  if (length(images) != length(masks)) stop("Give one mask per image.", call. = FALSE)
  rows <- .in_with_seed(seed, lapply(seq_along(images), function(i) {
    img <- .in_as_image(if (is.list(images)) images[[i]] else images[i])
    m <- .in_as_mask(if (is.list(masks)) masks[[i]] else masks[i])
    one <- function(cond, im, mm) cbind(id = i, condition = cond, in_traits(im, mm),
                                        stringsAsFactors = FALSE)
    out <- list(one("original", img, m))
    for (tp in types) {
      p <- in_augment(img, m, tp)
      if (sum(p$mask) >= 100) out[[length(out) + 1L]] <- one(tp, p$image, p$mask)
    }
    D <- 2 * sqrt(sum(m) / pi)
    for (r in rungs[rungs < D]) {
      s <- r / D
      nh <- max(8L, round(nrow(m) * s))
      nw <- max(8L, round(ncol(m) * s))
      mm <- .in_area_resize(m * 1, nh, nw) > 0.5
      if (sum(mm) >= 50) out[[length(out) + 1L]] <- one(paste0("D", r), .in_resize_image(img, nh, nw), mm)
    }
    do.call(rbind, out)
  }))
  long <- do.call(rbind, rows)
  tr <- .in_core_traits
  conds <- setdiff(unique(long$condition), "original")
  icc <- sapply(conds, function(cd) {
    sub <- long[long$condition %in% c("original", cd), ]
    both <- names(which(tapply(sub$condition, sub$id, function(z) length(unique(z))) == 2))
    sub <- sub[as.character(sub$id) %in% both, ]
    if (length(both) < min_n) return(stats::setNames(rep(NA_real_, length(tr)), tr))
    vapply(tr, function(t) {
      v <- sub[[t]]
      if (t %in% .in_circular) v <- .in_rotate(v)
      in_icc(v, sub$id)
    }, numeric(1))
  })
  out <- as.data.frame(matrix(icc, nrow = length(tr), dimnames = list(tr, conds)))
  attr(out, "data") <- long
  out
}

.in_with_seed <- function(seed, code) {
  if (exists(".Random.seed", envir = globalenv())) {
    old <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old, envir = globalenv()))
  } else {
    on.exit(rm(".Random.seed", envir = globalenv()))
  }
  set.seed(seed)
  code
}

.in_rotate <- function(v) {
  r <- v[is.finite(v)] * pi / 180
  if (!length(r)) return(v)
  cm <- atan2(mean(sin(r)), mean(cos(r))) * 180 / pi
  (v - cm + 180) %% 360
}

.in_to_srgb8 <- function(lin) {
  x <- pmin(pmax(lin, 0), 1)
  s <- ifelse(x <= 0.0031308, 12.92 * x, 1.055 * x^(1 / 2.4) - 0.055)
  floor(s * 255 + 0.5)
}

.in_gauss <- function(M, sigma) {
  k <- bitwOr(as.integer(round(sigma * 6 + 1)), 1L)
  r <- (k - 1) %/% 2
  wts <- exp(-((-r):r)^2 / (2 * sigma^2))
  wts <- wts / sum(wts)
  refl <- function(n) {
    i <- (-r):(n - 1 + r)
    if (n == 1L) return(rep(1L, length(i)))
    while (any(i < 0 | i > n - 1)) {
      i[i < 0] <- -i[i < 0]
      i[i > n - 1] <- 2 * (n - 1) - i[i > n - 1]
    }
    i + 1L
  }
  h <- nrow(M)
  w <- ncol(M)
  P <- M[, refl(w), drop = FALSE]
  o <- matrix(0, h, w)
  for (d in seq_along(wts)) o <- o + wts[d] * P[, d:(d + w - 1), drop = FALSE]
  P <- o[refl(h), , drop = FALSE]
  o <- matrix(0, h, w)
  for (d in seq_along(wts)) o <- o + wts[d] * P[d:(d + h - 1), , drop = FALSE]
  o
}

.in_homography <- function(src, dst) {
  A <- matrix(0, 8, 8)
  b <- numeric(8)
  for (i in 1:4) {
    x <- src[i, 1]
    y <- src[i, 2]
    u <- dst[i, 1]
    v <- dst[i, 2]
    A[i, ] <- c(x, y, 1, 0, 0, 0, -x * u, -y * u)
    A[i + 4, ] <- c(0, 0, 0, x, y, 1, -x * v, -y * v)
    b[i] <- u
    b[i + 4] <- v
  }
  matrix(c(solve(A, b), 1), 3, 3, byrow = TRUE)
}

# Inverse-mapping warp: bilinear interpolation with reflected borders for the
# image, nearest neighbour with an empty border for the mask.
.in_warp <- function(img, mask, H) {
  h <- nrow(mask)
  w <- ncol(mask)
  Hi <- solve(H)
  gx <- rep(0:(w - 1), each = h)
  gy <- rep(0:(h - 1), times = w)
  den <- Hi[3, 1] * gx + Hi[3, 2] * gy + Hi[3, 3]
  sx <- (Hi[1, 1] * gx + Hi[1, 2] * gy + Hi[1, 3]) / den
  sy <- (Hi[2, 1] * gx + Hi[2, 2] * gy + Hi[2, 3]) / den
  refl <- function(i, n) {
    for (it in 1:3) {
      i[i < 0] <- -i[i < 0] - 1
      i[i > n - 1] <- 2 * n - 1 - i[i > n - 1]
    }
    pmin(pmax(i, 0), n - 1)
  }
  x0 <- floor(sx)
  y0 <- floor(sy)
  fx <- sx - x0
  fy <- sy - y0
  X0 <- refl(x0, w)
  X1 <- refl(x0 + 1, w)
  Y0 <- refl(y0, h)
  Y1 <- refl(y0 + 1, h)
  out <- img
  for (ch in 1:3) {
    M <- img[, , ch]
    v <- (1 - fx) * (1 - fy) * M[cbind(Y0 + 1, X0 + 1)] + fx * (1 - fy) * M[cbind(Y0 + 1, X1 + 1)] +
      (1 - fx) * fy * M[cbind(Y1 + 1, X0 + 1)] + fx * fy * M[cbind(Y1 + 1, X1 + 1)]
    out[, , ch] <- matrix(round(v), h, w)
  }
  nx <- round(sx)
  ny <- round(sy)
  inside <- nx >= 0 & nx <= w - 1 & ny >= 0 & ny <= h - 1
  mv <- logical(length(nx))
  mv[inside] <- mask[cbind(ny[inside] + 1, nx[inside] + 1)]
  list(image = out, mask = matrix(mv, h, w))
}
