#' List the photos in a folder or in species folders
#'
#' Lists JPEG, PNG, TIFF, WebP and BMP files in a folder and its sub-folders.
#' When the photos are sorted into one sub-folder per species (for example
#' `photos/Apis mellifera/IMG_001.jpg` or `photos/Apis_mellifera/IMG_001.jpg`),
#' the sub-folder name becomes the species name. The table can be passed to
#' [in_process()]; [in_taxonomy()] adds the order and family.
#'
#' @param path Folder with photos, or with one sub-folder per species.
#' @param recursive Search sub-folders.
#' @param species_from_folder Take the species name from the name of the folder
#'   that holds each photo (underscores become spaces). Photos directly in
#'   `path` get `NA`.
#' @return A data frame with `photo_id` (unique, built from the relative path),
#'   `path`, `species` and `file`.
#' @seealso [in_taxonomy()], [in_process()]
#' @examples
#' d <- file.path(tempdir(), "species_photos")
#' ex <- in_example_photos()
#' for (i in 1:3) {
#'   dir.create(file.path(d, ex$species[i]), recursive = TRUE, showWarnings = FALSE)
#'   file.copy(ex$photo[i], file.path(d, ex$species[i], basename(ex$photo[i])))
#' }
#' in_read_folder(d)
#' @export
in_read_folder <- function(path, recursive = TRUE, species_from_folder = TRUE) {
  if (!dir.exists(path)) stop("Folder not found: ", path, call. = FALSE)
  rel <- list.files(path, pattern = "\\.(jpe?g|png|tiff?|webp|bmp)$", ignore.case = TRUE,
                    recursive = recursive)
  if (!length(rel)) stop("No images found in ", path, call. = FALSE)
  folder <- dirname(rel)
  species <- rep(NA_character_, length(rel))
  if (species_from_folder) {
    sub <- folder != "."
    species[sub] <- trimws(gsub("[_]+", " ", basename(folder[sub])))
  }
  id <- gsub("[^A-Za-z0-9._-]+", "_", tools::file_path_sans_ext(rel))
  id <- gsub("/", "__", id, fixed = TRUE)
  data.frame(photo_id = make.unique(id, sep = "_"), path = normalizePath(file.path(path, rel)),
             species = species, file = basename(rel), stringsAsFactors = FALSE)
}

#' Add order, family and class from the GBIF backbone
#'
#' Matches species names (for example folder names from [in_read_folder()])
#' with the GBIF backbone taxonomy. Names that are not insects get a warning,
#' because \pkg{InsectTrait} is validated for insects only.
#'
#' @param x A character vector of names, or a data frame with a `species`
#'   column.
#' @return For a vector, a data frame with `species`, `matched_name`, `rank`,
#'   `class`, `order`, `family` and `match_type`. For a data frame, `x` with
#'   `order`, `family` and `class` filled in where they were missing.
#' @examples
#' \dontrun{
#' in_taxonomy(c("Apis mellifera", "Vanessa atalanta"))
#' }
#' @export
in_taxonomy <- function(x) {
  nm <- if (is.data.frame(x)) x$species else x
  if (is.null(nm)) stop("`x` needs a `species` column.", call. = FALSE)
  u <- unique(stats::na.omit(as.character(nm)))
  rows <- lapply(u, function(s) {
    m <- tryCatch(.in_get_json("https://api.gbif.org/v1/species/match", list(name = s)),
                  error = function(e) list())
    g <- function(k) if (is.null(m[[k]])) NA_character_ else as.character(m[[k]])
    data.frame(species = s, matched_name = g("canonicalName"), rank = g("rank"), class = g("class"),
               order = g("order"), family = g("family"), match_type = g("matchType"),
               stringsAsFactors = FALSE)
  })
  tab <- if (length(rows)) do.call(rbind, rows) else
    data.frame(species = character(), matched_name = character(), rank = character(),
               class = character(), order = character(), family = character(),
               match_type = character(), stringsAsFactors = FALSE)
  bad <- tab$species[is.na(tab$class) | tab$class != "Insecta"]
  if (length(bad)) {
    warning("Not matched to an insect (InsectTrait is validated for insects only): ",
            paste(bad, collapse = ", "), call. = FALSE)
  }
  if (!is.data.frame(x)) return(tab)
  i <- match(x$species, tab$species)
  for (k in c("order", "family", "class")) {
    old <- if (k %in% names(x)) as.character(x[[k]]) else rep(NA_character_, nrow(x))
    x[[k]] <- ifelse(is.na(old) | old == "", tab[[k]][i], old)
  }
  x
}

#' Find insect photos on iNaturalist
#'
#' Searches the iNaturalist API for observations of an insect taxon with photos
#' and returns one row per photo, with the licence and attribution given by
#' iNaturalist. Research-grade observations and photos under CC0 or CC BY are
#' returned by default. Requests are spaced by one second to respect the API
#' rate limit. The taxon must belong to the class Insecta.
#'
#' @param taxon Scientific name or iNaturalist taxon id.
#' @param n Maximum number of photos.
#' @param place_id Optional iNaturalist place id.
#' @param year Optional year of observation.
#' @param license Photo licences to keep (iNaturalist codes such as `"cc0"`,
#'   `"cc-by"`, `"cc-by-nc"`).
#' @param quality_grade `"research"` (default), `"needs_id"` or `"any"`.
#' @param max_per_observation Maximum photos per observation.
#' @return A data frame with `photo_id`, `observation_id`, `species`, `order`,
#'   `family`, `observed_on`, `latitude`, `longitude`, `place_guess`, `url`
#'   (original size), `license`, `rightsHolder`, `attribution` and
#'   `observation_url`.
#' @seealso [in_download_photos()], [in_gbif_photos()]
#' @examples
#' \dontrun{
#' ph <- in_inat_photos("Harmonia axyridis", n = 20)
#' ph <- in_download_photos(ph, dir = "PATH_TO_PHOTO_FOLDER")
#' }
#' @export
in_inat_photos <- function(taxon, n = 100, place_id = NULL, year = NULL,
                           license = c("cc0", "cc-by"), quality_grade = "research",
                           max_per_observation = 1) {
  tx <- .in_inat_taxon(taxon)
  out <- list()
  got <- 0
  id_below <- NULL
  repeat {
    q <- list(taxon_id = tx$id, photos = "true", per_page = 200, order_by = "id", order = "desc",
              photo_license = paste(license, collapse = ","), place_id = place_id, year = year,
              id_below = id_below)
    if (quality_grade != "any") q$quality_grade <- quality_grade
    js <- .in_get_json("https://api.inaturalist.org/v1/observations", q)
    res <- js$results
    if (!length(res)) break
    d <- .in_parse_inat(res, license, max_per_observation)
    if (nrow(d)) {
      out[[length(out) + 1L]] <- d
      got <- got + nrow(d)
    }
    id_below <- min(vapply(res, function(r) as.numeric(r$id), 0))
    if (got >= n || length(res) < 200) break
    Sys.sleep(1)
  }
  res <- if (length(out)) do.call(rbind, out) else .in_parse_inat(list(), license, 1)
  res <- utils::head(res, n)
  if (nrow(res)) {
    tax <- .in_inat_ancestry(unique(res$taxon_id))
    res$order <- tax$order[match(res$taxon_id, tax$id)]
    res$family <- tax$family[match(res$taxon_id, tax$id)]
  }
  rownames(res) <- NULL
  res[, c("photo_id", "observation_id", "species", "order", "family", "observed_on", "latitude",
          "longitude", "place_guess", "url", "license", "rightsHolder", "attribution",
          "observation_url")]
}

.in_inat_taxon <- function(taxon) {
  if (is.numeric(taxon)) {
    t <- .in_get_json(paste0("https://api.inaturalist.org/v1/taxa/", taxon), list())$results
  } else {
    t <- .in_get_json("https://api.inaturalist.org/v1/taxa", list(q = taxon, per_page = 30))$results
    exact <- Filter(function(r) identical(tolower(r$name), tolower(trimws(taxon))), t)
    if (length(exact)) t <- exact
  }
  if (!length(t)) stop("No iNaturalist taxon found for '", taxon, "'.", call. = FALSE)
  t <- t[[1]]
  insect <- identical(t$iconic_taxon_name, "Insecta") || 47158 %in% unlist(t$ancestor_ids)
  if (!insect) {
    stop("'", t$name, "' is not an insect on iNaturalist. InsectTrait is validated for insects only.",
         call. = FALSE)
  }
  list(id = t$id, name = t$name)
}

.in_inat_ancestry <- function(ids) {
  ids <- ids[!is.na(ids)]
  rows <- list()
  for (chunk in split(ids, ceiling(seq_along(ids) / 30))) {
    res <- tryCatch(.in_get_json(paste0("https://api.inaturalist.org/v1/taxa/",
                                        paste(chunk, collapse = ",")), list())$results,
                    error = function(e) list())
    for (r in res) {
      anc <- c(r$ancestors, list(r))
      rk <- function(k) {
        hit <- Filter(function(a) identical(a$rank, k), anc)
        if (length(hit)) hit[[1]]$name else NA_character_
      }
      rows[[length(rows) + 1L]] <- data.frame(id = r$id, order = rk("order"), family = rk("family"),
                                              stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(data.frame(id = numeric(), order = character(), family = character()))
  do.call(rbind, rows)
}

.in_parse_inat <- function(res, license = c("cc0", "cc-by"), max_per_observation = 1) {
  g <- function(x, k) if (is.null(x[[k]])) NA else x[[k]]
  rows <- list()
  for (r in res) {
    k <- 0
    for (ph in r$photos) {
      lic <- tolower(as.character(g(ph, "license_code")))
      if (is.na(lic) || !lic %in% license) next
      k <- k + 1
      if (k > max_per_observation) break
      loc <- suppressWarnings(as.numeric(strsplit(as.character(g(r, "location")), ",")[[1]]))
      who <- g(r$user, "name")
      if (is.na(who) || !nzchar(who)) who <- g(r$user, "login")
      rows[[length(rows) + 1L]] <- data.frame(
        photo_id = sprintf("inat%s_%02d", format(r$id, scientific = FALSE), k),
        observation_id = format(r$id, scientific = FALSE),
        species = as.character(g(r$taxon, "name")), taxon_id = as.numeric(g(r$taxon, "id")),
        observed_on = as.character(g(r, "observed_on")),
        latitude = if (length(loc) == 2) loc[1] else NA_real_,
        longitude = if (length(loc) == 2) loc[2] else NA_real_,
        place_guess = as.character(g(r, "place_guess")),
        url = sub("/square\\.", "/original.", as.character(ph$url)),
        license = lic, rightsHolder = as.character(who),
        attribution = as.character(g(ph, "attribution")),
        observation_url = paste0("https://www.inaturalist.org/observations/",
                                 format(r$id, scientific = FALSE)),
        stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) {
    return(data.frame(photo_id = character(), observation_id = character(), species = character(),
                      taxon_id = numeric(), observed_on = character(), latitude = numeric(),
                      longitude = numeric(), place_guess = character(), url = character(),
                      license = character(), rightsHolder = character(), attribution = character(),
                      observation_url = character(), order = character(), family = character(),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}
