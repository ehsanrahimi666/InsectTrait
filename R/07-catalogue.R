#' Look up trait definitions and reliability
#'
#' Returns rows of [in_catalogue]: the definition, trait family, how the trait
#' is computed, its photo-to-photo repeatability, invariance, minimum insect
#' size and the reliability verdict.
#'
#' @param trait Optional trait names.
#' @param family Optional trait family: `"colour"`, `"pattern"` or `"shape"`.
#' @param verdict Optional verdict: `"reliable"`, `"use with care"` or
#'   `"not reliable"`.
#' @param columns Columns to return; `NULL` returns all.
#' @return A data frame.
#' @examples
#' in_trait_info(c("L_mean", "hue_deg"))
#' in_trait_info(verdict = "not reliable")
#' table(in_catalogue$family, in_catalogue$verdict)
#' @export
in_trait_info <- function(trait = NULL, family = NULL, verdict = NULL,
                          columns = c("trait", "family", "verdict", "reason", "icc_default",
                                      "inv_photometric_min", "min_D_px", "computed_by",
                                      "description")) {
  x <- in_catalogue
  if (!is.null(trait)) {
    bad <- setdiff(trait, x$trait)
    if (length(bad)) stop("Unknown trait: ", paste(bad, collapse = ", "), call. = FALSE)
    x <- x[match(trait, x$trait), ]
  }
  if (!is.null(family)) x <- x[x$family %in% family, ]
  if (!is.null(verdict)) x <- x[as.character(x$verdict) %in% verdict, ]
  if (!is.null(columns)) x <- x[, columns, drop = FALSE]
  rownames(x) <- NULL
  x
}

#' Names of reliable traits
#'
#' Selects traits by their validation verdict and, optionally, by the minimum
#' insect size at which they stayed stable (ICC of at least 0.8 against the
#' full-resolution value).
#'
#' @param verdict Verdicts to include.
#' @param family Optional trait family.
#' @param insect_px Optional typical insect size in your photos (equivalent
#'   diameter `D_px`, pixels). Traits whose minimum size is larger, or unknown,
#'   are dropped.
#' @return A character vector of trait names.
#' @examples
#' in_reliable_traits()
#' in_reliable_traits(c("reliable", "use with care"), family = "pattern")
#' in_reliable_traits(insect_px = 64)
#' @export
in_reliable_traits <- function(verdict = "reliable", family = NULL, insect_px = NULL) {
  x <- in_catalogue[as.character(in_catalogue$verdict) %in% verdict, ]
  if (!is.null(family)) x <- x[x$family %in% family, ]
  if (!is.null(insect_px)) x <- x[!is.na(x$min_D_px) & x$min_D_px <= insect_px, ]
  x$trait
}
