#' Read a photo
#'
#' Reads a JPEG or PNG file into an RGB array with values from 0 to 255.
#' Grey-scale images are expanded to three channels and an alpha channel is
#' dropped. The file type is taken from the file signature, not the extension.
#'
#' @param path Path to a JPEG or PNG file.
#' @param max_side Optional maximum length (pixels) of the longer side. Larger
#'   images are reduced by area averaging, as in the back end.
#' @return A numeric array with dimensions height x width x 3 (values 0-255).
#' @seealso [in_read_mask()], [in_traits()]
#' @examples
#' ex <- in_example_photos()
#' img <- in_read_image(ex$photo[1])
#' dim(img)
#' small <- in_read_image(ex$photo[1], max_side = 200)
#' dim(small)
#' @export
in_read_image <- function(path, max_side = NULL) {
  x <- .in_read_raw(path)
  if (length(dim(x)) == 2L) x <- array(x, c(dim(x), 3L))
  if (dim(x)[3] == 2L) x <- x[, , c(1, 1, 1), drop = FALSE]
  if (dim(x)[3] == 4L) x <- x[, , 1:3, drop = FALSE]
  img <- round(x * 255)
  if (!is.null(max_side)) img <- .in_shrink(img, max_side)
  img
}

#' Read and write insect outline masks
#'
#' Masks are logical matrices, `TRUE` for insect pixels. `in_read_mask()`
#' treats any non-zero PNG pixel as insect (so 0/1 and 0/255 masks both work);
#' for JPEG masks a threshold of 128 is used. `in_write_mask()` writes a PNG
#' with values 0 and 255, the format used by the back end.
#'
#' @param path Path to a PNG (preferred) or JPEG mask.
#' @param mask Logical (or 0/1) matrix.
#' @return `in_read_mask()`: a logical matrix. `in_write_mask()`: `path`,
#'   invisibly.
#' @examples
#' ex <- in_example_photos()
#' m <- in_read_mask(ex$mask[1])
#' sum(m)
#' f <- tempfile(fileext = ".png")
#' in_write_mask(m, f)
#' identical(in_read_mask(f), m)
#' @export
in_read_mask <- function(path) {
  x <- .in_read_raw(path)
  if (length(dim(x)) == 3L) x <- x[, , 1]
  if (.in_format(path) == "jpeg") x > 0.5 else x > 0
}

#' @rdname in_read_mask
#' @export
in_write_mask <- function(mask, path) {
  if (!is.matrix(mask)) stop("`mask` must be a matrix.", call. = FALSE)
  png::writePNG(matrix(as.numeric(mask != 0), nrow(mask)), path)
  invisible(path)
}

#' Example insect photos shipped with the package
#'
#' Seven research-grade 'iNaturalist' photos (CC0 or CC BY 4.0) of insects from
#' four orders, reduced to 800 pixels on the longer side, with the crops and
#' outlines produced by the back end. The table gives the rights holder and
#' licence of each photo; please keep this attribution when you reuse them.
#'
#' @return A data frame with one row per photo: `photo_id`, `species`, `order`,
#'   `family`, `occurrenceKey`, `gbif_url`, `image_url`, `rights_holder`,
#'   `license`, `country`, `year`, `modification` and the full paths `photo`,
#'   `crop` and `mask`.
#' @seealso [in_example_results()]
#' @examples
#' ex <- in_example_photos()
#' ex[, c("photo_id", "species", "license", "rights_holder")]
#' @export
in_example_photos <- function() {
  d <- system.file("extdata", package = "InsectTrait")
  x <- utils::read.csv(file.path(d, "ATTRIBUTION.csv"), stringsAsFactors = FALSE,
                       colClasses = c(photo_id = "character", occurrenceKey = "character"))
  x$photo <- file.path(d, "photos", paste0(x$photo_id, ".jpg"))
  x$crop <- file.path(d, "crops", paste0(x$photo_id, ".jpg"))
  x$mask <- file.path(d, "masks", paste0(x$photo_id, ".png"))
  x
}

#' Back-end results for the example photos
#'
#' The output of [in_process()] for the seven photos in [in_example_photos()],
#' read with [in_read_results()], so that filtering, summaries and plots can
#' be tried without 'Python'.
#'
#' @inheritParams in_filter
#' @return A `in_results` data frame (see [in_read_results()]).
#' @examples
#' res <- in_example_results()
#' res[, c("photo_id", "species", "keep", "reason")]
#' @export
in_example_results <- function(rule = "default") {
  f <- system.file("extdata", "example_results.csv", package = "InsectTrait")
  res <- in_read_results(f, rule = rule)
  attr(res, "dir") <- system.file("extdata", package = "InsectTrait")
  res
}

.in_format <- function(path) {
  sig <- readBin(path, "raw", n = 8L)
  if (length(sig) >= 3L && identical(sig[1:3], as.raw(c(0xFF, 0xD8, 0xFF)))) return("jpeg")
  png_sig <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A))
  if (length(sig) == 8L && identical(sig, png_sig)) return("png")
  "other"
}

.in_read_raw <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    stop("File not found: ", paste(path, collapse = ", "), call. = FALSE)
  }
  switch(.in_format(path),
         jpeg = jpeg::readJPEG(path),
         png = png::readPNG(path),
         stop("Only JPEG and PNG files are supported: ", path, call. = FALSE))
}

.in_as_image <- function(img) {
  if (is.character(img)) img <- in_read_image(img)
  d <- dim(img)
  if (!is.numeric(img) || length(d) != 3L || d[3] != 3L) {
    stop("`img` must be a height x width x 3 array (values 0-255) or a file path.",
         call. = FALSE)
  }
  if (max(img, na.rm = TRUE) <= 1 && any(img > 0)) {
    warning("`img` looks scaled to 0-1; InsectTrait expects 0-255.", call. = FALSE)
  }
  img
}

.in_as_mask <- function(mask) {
  if (is.character(mask)) mask <- in_read_mask(mask)
  if (!is.matrix(mask)) stop("`mask` must be a matrix or a file path.", call. = FALSE)
  m <- mask != 0
  m[is.na(m)] <- FALSE
  m
}

# Area-averaging resize along the first dimension (exact pixel-overlap weights,
# computed from cumulative sums).
.in_area_rows <- function(M, n_out) {
  n_in <- nrow(M)
  if (n_out == n_in) return(M)
  s <- n_in / n_out
  C <- rbind(0, apply(M, 2, cumsum))
  e <- (0:n_out) * s
  k <- pmin(floor(e), n_in - 1)
  S <- C[k + 1, , drop = FALSE] + (e - k) * M[k + 1, , drop = FALSE]
  (S[-1, , drop = FALSE] - S[-(n_out + 1), , drop = FALSE]) / s
}

.in_area_resize <- function(M, h, w) {
  t(.in_area_rows(t(.in_area_rows(M, h)), w))
}

.in_resize_image <- function(img, h, w) {
  out <- array(0, c(h, w, 3L))
  for (ch in 1:3) out[, , ch] <- .in_area_resize(img[, , ch], h, w)
  round(out)
}

.in_shrink <- function(img, max_side) {
  s <- max_side / max(dim(img)[1:2])
  if (s >= 1) return(img)
  .in_resize_image(img, max(1L, round(dim(img)[1] * s)), max(1L, round(dim(img)[2] * s)))
}
