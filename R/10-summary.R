#' Summarise traits by group
#'
#' Mean, standard deviation and number of photos per group (for example per
#' species), using kept photos only by default. Hue angles are averaged on the
#' circle; their spread is the circular standard deviation in degrees.
#'
#' @param x Data frame of traits.
#' @param by Grouping column.
#' @param traits Trait columns; by default the catalogue traits rated reliable
#'   that are present in `x`.
#' @param keep_only Use only rows with `keep == TRUE`.
#' @return A data frame with one row per group: `n` and, for each trait,
#'   `<trait>_mean` and `<trait>_sd`.
#' @examples
#' v <- in_filter(in_validation, "default")
#' s <- in_summarise(v, by = "species", traits = c("L_mean", "C_mean", "hue_deg"))
#' head(s)
#' @export
in_summarise <- function(x, by = "species", traits = NULL, keep_only = TRUE) {
  if (!by %in% names(x)) stop("Column `", by, "` not found.", call. = FALSE)
  if (keep_only && "keep" %in% names(x)) x <- x[x$keep %in% TRUE, , drop = FALSE]
  if (is.null(traits)) traits <- intersect(in_reliable_traits(), names(x))
  miss <- setdiff(traits, names(x))
  if (length(miss)) stop("Traits not found: ", paste(miss, collapse = ", "), call. = FALSE)
  g <- factor(x[[by]])
  out <- data.frame(group = levels(g), n = as.integer(table(g)), stringsAsFactors = FALSE)
  names(out)[1] <- by
  for (t in traits) {
    v <- as.numeric(x[[t]])
    if (t %in% .in_circular) {
      r <- v * pi / 180
      s <- tapply(sin(r), g, mean, na.rm = TRUE)
      cc <- tapply(cos(r), g, mean, na.rm = TRUE)
      out[[paste0(t, "_mean")]] <- as.numeric((atan2(s, cc) * 180 / pi) %% 360)
      out[[paste0(t, "_sd")]] <- as.numeric(sqrt(-2 * log(pmin(1, sqrt(s^2 + cc^2)))) * 180 / pi)
    } else {
      out[[paste0(t, "_mean")]] <- as.numeric(tapply(v, g, mean, na.rm = TRUE))
      out[[paste0(t, "_sd")]] <- as.numeric(tapply(v, g, stats::sd, na.rm = TRUE))
    }
  }
  out
}
