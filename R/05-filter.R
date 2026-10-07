#' Keep usable photos with a validated decision rule
#'
#' Applies one of the decision rules evaluated on the 480 benchmark photos of
#' the validation. Checks are made in this order and the first failed check
#' gives the `reason`:
#' \enumerate{
#'   \item processing error;
#'   \item outline quality (`mask_ok`: a detection, at least 600 outline pixels
#'     and at most 10% of the crop border touched);
#'   \item outline stability `stab >= stability` (rules `"strict"` and
#'     `"top_view"` only);
#'   \item a single insect (`n_other_insects == 0`);
#'   \item photo filter `p_usable_any >= p_usable` (the insect is visible,
#'     sharp and not cut off);
#'   \item rule `"top_view"` only: top view (`p_top_view >= p_top`) and, for
#'     Lepidoptera, the upper wing side (`p_upperside_lep >= p_upper`).
#' }
#' Kept photos get a `view`: `"top"`, `"side"` or, for Lepidoptera with
#' `p_upperside_lep < p_upper`, `"underside"`.
#'
#' In the validation (480 benchmark photos with rated outlines), the rules kept, and
#' left wrong outlines in: no rule 100% (4.2% wrong); `"qc"` 99% (3.6%);
#' `"default"` 71% (0.9%); `"strict"` 56% (0.4%); `"top_view"` 40%
#' (0.5%). See [in_validation].
#'
#' @param x Data frame with the back-end columns (`mask_ok`, `stab`,
#'   `n_other_insects`, `p_usable_any`, `p_top_view`, `p_upperside_lep` and,
#'   for the Lepidoptera rule, `order`), such as [in_read_results()] or
#'   [in_validation].
#' @param rule `"default"` (outline QC, one insect, photo filter), `"strict"`
#'   (default plus outline stability), `"top_view"` (strict plus top view and
#'   upper wing side), `"qc"` (outline QC only) or `"none"`.
#' @param p_usable,stability,p_top,p_upper Thresholds; the validated values are
#'   0.5, 0.8, 0.5 and 0.5.
#' @return `x` with the columns `keep`, `reason` and `view`.
#' @examples
#' res <- in_example_results()
#' in_filter(res, "strict")[, c("photo_id", "keep", "reason")]
#'
#' # the validation photos: share kept by each rule
#' sapply(c("qc", "default", "strict", "top_view"),
#'        function(r) mean(in_filter(in_validation, r)$keep))
#' @export
in_filter <- function(x, rule = c("default", "strict", "top_view", "qc", "none"),
                      p_usable = 0.5, stability = 0.8, p_top = 0.5, p_upper = 0.5) {
  rule <- match.arg(rule)
  if (!is.data.frame(x)) stop("`x` must be a data frame.", call. = FALSE)
  n <- nrow(x)
  need <- switch(rule, none = character(), qc = "mask_ok",
                 default = c("mask_ok", "n_other_insects", "p_usable_any"),
                 c("mask_ok", "stab", "n_other_insects", "p_usable_any",
                   "p_top_view", "p_upperside_lep"))
  miss <- setdiff(need, names(x))
  if (length(miss)) {
    stop("Columns missing for rule \"", rule, "\": ", paste(miss, collapse = ", "), call. = FALSE)
  }
  num <- function(k, fill) {
    v <- if (k %in% names(x)) suppressWarnings(as.numeric(x[[k]])) else rep(fill, n)
    v[is.na(v)] <- fill
    v
  }
  reason <- rep(NA_character_, n)
  fail <- function(cond, why) {
    i <- is.na(reason) & cond
    reason[i] <<- why
  }
  if ("error" %in% names(x)) {
    e <- as.character(x$error)
    fail(!is.na(e) & nzchar(trimws(e)), "processing error")
  }
  if (rule != "none") {
    ok <- .in_lgl(x$mask_ok)
    fail(is.na(ok) | !ok, "outline failed QC")
  }
  if (rule %in% c("strict", "top_view")) fail(num("stab", 0) < stability, "unstable outline")
  if (rule %in% c("default", "strict", "top_view")) {
    fail(num("n_other_insects", 0) > 0, "several insects")
    fail(num("p_usable_any", 0) < p_usable, "unclear photo (hidden, blurry or cut off)")
  }
  lep <- if ("order" %in% names(x)) !is.na(x$order) & x$order == "Lepidoptera" else rep(FALSE, n)
  view <- rep(NA_character_, n)
  if (all(c("p_top_view", "p_upperside_lep") %in% names(x))) {
    view <- ifelse(lep & num("p_upperside_lep", 1) < p_upper, "underside",
                   ifelse(num("p_top_view", 1) >= p_top, "top", "side"))
  }
  if (rule == "top_view") {
    fail(view == "underside", "underside view")
    fail(view == "side", "side view")
  }
  x$keep <- is.na(reason)
  view[!x$keep] <- NA
  reason[x$keep] <- if (all(is.na(view))) "kept" else paste(view[x$keep], "view")
  x$reason <- reason
  x$view <- view
  if (!inherits(x, "in_results")) class(x) <- c("in_results", class(x))
  attr(x, "rule") <- rule
  x
}

#' @export
`[.in_results` <- function(x, ...) {
  r <- NextMethod()
  if (!is.data.frame(r)) return(r)
  if (all(c("photo_id", "keep") %in% names(r))) {
    attr(r, "rule") <- attr(x, "rule")
    attr(r, "dir") <- attr(x, "dir")
  } else {
    class(r) <- setdiff(class(r), "in_results")
  }
  r
}

#' @export
print.in_results <- function(x, ...) {
  cat("<in_results> ", nrow(x), " photos, ", sum(x$keep, na.rm = TRUE), " kept (rule: ",
      attr(x, "rule") %||% "?", ")\n", sep = "")
  show <- intersect(c("photo_id", "species", "order", "keep", "reason", "D_px", "L_mean",
                      "C_mean", "dark_frac_otsu"), names(x))
  print(utils::head(as.data.frame(x)[, show, drop = FALSE], 10L), row.names = FALSE)
  if (nrow(x) > 10L) cat("... ", nrow(x) - 10L, " more rows\n", sep = "")
  invisible(x)
}

#' Summarise or plot the decisions in a results table
#'
#' @param object,x A `in_results` data frame.
#' @param ... Passed to [graphics::barplot()] (plot) or ignored (summary).
#' @return `summary()`: a data frame with the number and share of photos per
#'   decision. `plot()`: the same table, invisibly, after drawing a bar chart.
#' @examples
#' res <- in_example_results()
#' summary(res)
#' plot(res)
#' @name in_results
NULL

#' @rdname in_results
#' @export
summary.in_results <- function(object, ...) {
  tab <- table(factor(object$reason))
  out <- data.frame(reason = names(tab), n = as.integer(tab), share = as.numeric(tab) / nrow(object),
                    stringsAsFactors = FALSE)
  out$kept <- out$reason %in% unique(object$reason[object$keep])
  out <- out[order(!out$kept, -out$n), c("reason", "kept", "n", "share")]
  rownames(out) <- NULL
  out
}

#' @rdname in_results
#' @export
plot.in_results <- function(x, ...) {
  s <- summary.in_results(x)
  op <- graphics::par(mar = c(4, 16, 2, 1))
  on.exit(graphics::par(op))
  graphics::barplot(rev(s$n), names.arg = rev(s$reason), horiz = TRUE, las = 1,
                    col = rev(ifelse(s$kept, "#2E8B57", "#C0504D")), border = NA,
                    xlab = "Photos", main = paste0("Decisions (rule: ", attr(x, "rule"), ")"), ...)
  invisible(s)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
