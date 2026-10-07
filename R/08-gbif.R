.in_inat_key <- "50c9509d-22c7-4a22-a47d-8c48425ef4a7"
.in_ua <- "InsectTrait R package (https://github.com/ehsanrahimi666/InsectTrait)"

#' Find insect photos on GBIF
#'
#' Searches the GBIF occurrence API for photographed occurrences of an insect
#' taxon (by default research-grade 'iNaturalist' observations) and returns
#' one row per photo with its URL, licence and rights holder. The taxon must
#' belong to the class Insecta: \pkg{InsectTrait} is validated for insects
#' only.
#'
#' @param taxon Scientific name (matched with the GBIF backbone) or a GBIF
#'   taxon key.
#' @param n Maximum number of photos to return.
#' @param country Optional ISO 3166-1 alpha-2 country code, for example `"KR"`.
#' @param year Optional year or range such as `"2020,2025"`.
#' @param license Photo licences to keep (GBIF codes `"CC0_1_0"`, `"CC_BY_4_0"`,
#'   `"CC_BY_NC_4_0"`). The default keeps photos that may be reused with
#'   attribution, including in publications.
#' @param dataset_key GBIF dataset key; the default is 'iNaturalist'
#'   research-grade observations. `NULL` searches all datasets.
#' @param max_per_occurrence Maximum photos per occurrence. The validated
#'   workflow analyses one photo per insect and uses extra photos only for
#'   repeatability.
#' @param page_size Records per API request (at most 300).
#' @return A data frame with `photo_id`, `occurrenceKey`, `species`, `order`,
#'   `family`, `country`, `year`, `eventDate`, `decimalLatitude`,
#'   `decimalLongitude`, `url`, `license`, `rightsHolder`, `references` and
#'   `gbif_url`.
#' @seealso [in_download_photos()], [in_attribution()]
#' @examples
#' \dontrun{
#' ph <- in_gbif_photos("Harmonia axyridis", n = 20, country = "KR")
#' head(ph)
#' }
#' @export
in_gbif_photos <- function(taxon, n = 100, country = NULL, year = NULL,
                           license = c("CC0_1_0", "CC_BY_4_0"),
                           dataset_key = .in_inat_key, max_per_occurrence = 1,
                           page_size = 300) {
  key <- if (is.numeric(taxon)) {
    .in_check_insect(.in_get_json(paste0("https://api.gbif.org/v1/species/", taxon), list()))
  } else {
    .in_check_insect(.in_get_json("https://api.gbif.org/v1/species/match", list(name = taxon)))
  }
  out <- list()
  got <- 0
  offset <- 0
  repeat {
    q <- c(list(taxonKey = key, mediaType = "StillImage", limit = min(page_size, 300),
                offset = offset, datasetKey = dataset_key, country = country, year = year),
           stats::setNames(as.list(license), rep("license", length(license))))
    js <- .in_get_json("https://api.gbif.org/v1/occurrence/search", q)
    d <- .in_parse_occurrences(js, license, max_per_occurrence)
    if (nrow(d)) {
      out[[length(out) + 1L]] <- d
      got <- got + nrow(d)
    }
    offset <- offset + length(js$results)
    if (got >= n || isTRUE(js$endOfRecords) || !length(js$results)) break
  }
  res <- if (length(out)) do.call(rbind, out) else .in_parse_occurrences(list(results = list()))
  res <- utils::head(res, n)
  rownames(res) <- NULL
  res
}

#' Download photos
#'
#' Downloads the photos listed by [in_gbif_photos()] into a folder, named by
#' `photo_id`. Files already present are skipped.
#'
#' @param x Data frame with `photo_id` and `url` columns.
#' @param dir Destination folder.
#' @param size For 'iNaturalist' URLs: `"original"` (validated; longer side up
#'   to 2048 px), `"large"` (1024 px) or `"medium"` (500 px).
#' @param overwrite Download again if the file exists.
#' @return `x` with a `path` column (`NA` where the download failed).
#' @examples
#' \dontrun{
#' ph <- in_gbif_photos("Aglais io", n = 5)
#' ph <- in_download_photos(ph, dir = "PATH_TO_PHOTO_FOLDER")
#' }
#' @export
in_download_photos <- function(x, dir, size = c("original", "large", "medium"),
                               overwrite = FALSE) {
  size <- match.arg(size)
  if (!all(c("photo_id", "url") %in% names(x))) {
    stop("`x` needs `photo_id` and `url` columns.", call. = FALSE)
  }
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  url <- sub("/original\\.", paste0("/", size, "."), x$url)
  path <- file.path(dir, paste0(x$photo_id, ".jpg"))
  ok <- logical(nrow(x))
  for (i in seq_len(nrow(x))) {
    if (file.exists(path[i]) && !overwrite) {
      ok[i] <- TRUE
      next
    }
    for (attempt in 1:2) {
      ok[i] <- tryCatch({
        curl::curl_download(url[i], path[i], handle = .in_handle(), quiet = TRUE)
        TRUE
      }, error = function(e) FALSE)
      if (ok[i]) break
      Sys.sleep(1)
    }
    if (!ok[i] && file.exists(path[i])) unlink(path[i])
  }
  if (any(!ok)) warning(sum(!ok), " photo(s) could not be downloaded.", call. = FALSE)
  x$path <- ifelse(ok, normalizePath(path, mustWork = FALSE), NA_character_)
  x
}

#' Attribution lines for photos
#'
#' Builds the attribution that CC BY licences require when photos are shown,
#' for example in figures or supplementary material.
#'
#' @param x Data frame from [in_gbif_photos()], [in_inat_photos()] or
#'   [in_example_photos()].
#' @return A character vector, one line per photo.
#' @examples
#' in_attribution(in_example_photos())
#' @export
in_attribution <- function(x) {
  who <- if ("rightsHolder" %in% names(x)) x$rightsHolder else x$rights_holder
  lab <- c(CC0_1_0 = "CC0 1.0", CC_BY_4_0 = "CC BY 4.0",
           CC_BY_NC_4_0 = "CC BY-NC 4.0")[.in_license_code(x$license)]
  lab[is.na(lab)] <- x$license[is.na(lab)]
  out <- paste0(x$species, ". Photo: ", who, " (", unname(lab), "), iNaturalist via GBIF, ",
                "https://www.gbif.org/occurrence/", x$occurrenceKey)
  if (all(c("attribution", "observation_url") %in% names(x))) {
    inat <- !is.na(x$attribution) & nzchar(x$attribution)
    out[inat] <- paste0(x$species[inat], ". Photo: ", x$attribution[inat], ", iNaturalist, ",
                        x$observation_url[inat])
  }
  out
}

.in_get_json <- function(url, query) {
  query <- query[!vapply(query, is.null, logical(1))]
  qs <- paste(vapply(names(query), function(k) curl::curl_escape(k), ""),
              vapply(query, function(v) curl::curl_escape(as.character(v)), ""),
              sep = "=", collapse = "&")
  full <- if (length(query)) paste0(url, "?", qs) else url
  r <- NULL
  for (attempt in 1:3) {
    r <- tryCatch(curl::curl_fetch_memory(full, handle = .in_handle()), error = function(e) e)
    ok <- !inherits(r, "error") && r$status_code == 200
    retry <- inherits(r, "error") || r$status_code == 429 || r$status_code >= 500
    if (ok || !retry) break
    Sys.sleep(2^(attempt - 1))
  }
  if (inherits(r, "error")) {
    stop("GBIF could not be reached (", conditionMessage(r), "): ", full, call. = FALSE)
  }
  if (r$status_code != 200) {
    stop("GBIF request failed (HTTP ", r$status_code, "): ", full, call. = FALSE)
  }
  jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE)
}

.in_handle <- function() {
  curl::new_handle(useragent = .in_ua, connecttimeout = 30, timeout = 300)
}

.in_check_insect <- function(m) {
  key <- m$usageKey %||% m$key
  if (is.null(key) || identical(m$matchType, "NONE")) {
    stop("No GBIF backbone match for this name.", call. = FALSE)
  }
  if (!identical(m$class, "Insecta")) {
    stop("'", m$scientificName, "' is not an insect (class: ", m$class %||% "unknown",
         "). InsectTrait is validated for insects only.", call. = FALSE)
  }
  key
}

.in_license_code <- function(u) {
  u <- tolower(trimws(ifelse(is.na(u), "", u)))
  ifelse(grepl("publicdomain/zero|cc0", u), "CC0_1_0",
  ifelse(grepl("licenses/by/|cc_by_4_0|^cc[- ]by( 4\\.0)?$", u), "CC_BY_4_0",
  ifelse(grepl("licenses/by-nc/|cc_by_nc_4_0|^cc[- ]by-nc( 4\\.0)?$", u), "CC_BY_NC_4_0", "other")))
}

.in_parse_occurrences <- function(js, license = c("CC0_1_0", "CC_BY_4_0"), max_per_occurrence = 1) {
  g <- function(r, k) if (is.null(r[[k]])) NA else r[[k]]
  rows <- list()
  for (r in js$results) {
    media <- Filter(function(m) identical(m$type, "StillImage") && !is.null(m$identifier), r$media)
    k <- 0
    for (m in media) {
      lic <- .in_license_code(g(m, "license"))
      if (!lic %in% license) next
      k <- k + 1
      if (k > max_per_occurrence) break
      who <- g(m, "rightsHolder")
      if (is.na(who)) who <- g(m, "creator")
      rows[[length(rows) + 1L]] <- data.frame(
        photo_id = sprintf("%s_%02d", format(g(r, "key"), scientific = FALSE), k),
        occurrenceKey = format(g(r, "key"), scientific = FALSE),
        species = g(r, "species"), order = g(r, "order"), family = g(r, "family"),
        country = g(r, "countryCode"), year = as.integer(g(r, "year")),
        eventDate = as.character(g(r, "eventDate")),
        decimalLatitude = as.numeric(g(r, "decimalLatitude")),
        decimalLongitude = as.numeric(g(r, "decimalLongitude")),
        url = m$identifier, license = lic, rightsHolder = who,
        references = as.character(g(m, "references")),
        gbif_url = paste0("https://www.gbif.org/occurrence/", format(g(r, "key"), scientific = FALSE)),
        stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) {
    return(data.frame(photo_id = character(), occurrenceKey = character(), species = character(),
                      order = character(), family = character(), country = character(),
                      year = integer(), eventDate = character(), decimalLatitude = numeric(),
                      decimalLongitude = numeric(), url = character(), license = character(),
                      rightsHolder = character(), references = character(),
                      gbif_url = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}
