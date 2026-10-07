.in_py_packages <- c("numpy", "pandas", "opencv-python-headless", "pillow", "requests",
                     "torch", "torchvision", "transformers>=4.45", "ultralytics>=8.3",
                     "open_clip_torch>=2.24")
.in_py_modules <- c("numpy", "pandas", "cv2", "PIL", "requests", "torch", "transformers",
                    "ultralytics", "open_clip")

#' Install the 'Python' back end
#'
#' Creates a 'Python' environment and installs the packages that the photo back
#' end needs: 'PyTorch', 'transformers' (Grounding DINO, DINOv2),
#' 'ultralytics' (SAM 2.1), 'open_clip_torch' and 'OpenCV'. This is needed only
#' for [in_process()] and [in_extended_traits()]; everything else in
#' \pkg{InsectTrait} runs in R alone. Installation downloads about 2-3 GB, and
#' the first run downloads the model weights (about 1.5 GB).
#'
#' @param envname Name of the environment.
#' @param method `"virtualenv"` (default) or `"conda"`.
#' @param python Optional path to the 'Python' executable used to create a
#'   virtual environment (version 3.9 or later).
#' @param cpu_only If `TRUE`, install the CPU build of 'PyTorch' (smaller; no
#'   GPU support).
#' @param pip_options Extra options passed to `pip install`.
#' @return `envname`, invisibly.
#' @seealso [in_use_backend()], [in_backend_available()]
#' @examples
#' \dontrun{
#' in_install()                 # once
#' in_install(cpu_only = TRUE)  # laptop without a GPU
#' }
#' @export
in_install <- function(envname = "r-InsectTrait", method = c("virtualenv", "conda"),
                       python = NULL, cpu_only = FALSE, pip_options = character()) {
  method <- match.arg(method)
  pkgs <- .in_py_packages
  if (method == "virtualenv") {
    if (!reticulate::virtualenv_exists(envname)) {
      args <- list(envname = envname)
      if (!is.null(python)) args$python <- python
      do.call(reticulate::virtualenv_create, args)
    }
    if (cpu_only) {
      reticulate::virtualenv_install(envname, c("torch", "torchvision"),
                                     pip_options = c("--index-url",
                                                     "https://download.pytorch.org/whl/cpu"))
      pkgs <- setdiff(pkgs, c("torch", "torchvision"))
    }
    reticulate::virtualenv_install(envname, pkgs, pip_options = pip_options)
  } else {
    if (!envname %in% reticulate::conda_list()$name) {
      reticulate::conda_create(envname, python_version = "3.11")
    }
    if (cpu_only) {
      reticulate::conda_install(envname, c("torch", "torchvision"), pip = TRUE,
                                pip_options = c("--index-url",
                                                "https://download.pytorch.org/whl/cpu"))
      pkgs <- setdiff(pkgs, c("torch", "torchvision"))
    }
    reticulate::conda_install(envname, pkgs, pip = TRUE, pip_options = pip_options)
  }
  message("Installed. In a new R session call in_use_backend(\"", envname, "\").")
  invisible(envname)
}

#' Select and check the 'Python' back end
#'
#' `in_use_backend()` selects the environment made by [in_install()] for the
#' rest of the R session (it sets the option `InsectTrait.python`).
#' `in_backend_available()` reports whether all back-end modules can be
#' imported. The back end runs in a separate 'Python' process, so 'PyTorch' is
#' never loaded into the R session. Without `in_use_backend()`, the 'Python'
#' found by 'reticulate' is used.
#'
#' @inheritParams in_install
#' @param python Path to a 'Python' executable with the back-end packages
#'   (overrides `envname`).
#' @return `in_use_backend()`: the path of the 'Python' executable, invisibly.
#'   `in_backend_available()`: `TRUE` or `FALSE`.
#' @examples
#' \dontrun{
#' in_use_backend()
#' in_backend_available()
#' }
#' @export
in_use_backend <- function(envname = "r-InsectTrait", method = c("virtualenv", "conda"),
                           python = NULL) {
  method <- match.arg(method)
  if (is.null(python)) {
    python <- if (method == "virtualenv") {
      reticulate::virtualenv_python(envname)
    } else {
      reticulate::conda_python(envname)
    }
  }
  if (!file.exists(python)) stop("Python not found: ", python, call. = FALSE)
  options(InsectTrait.python = python)
  invisible(python)
}

#' @rdname in_use_backend
#' @export
in_backend_available <- function(python = NULL) {
  py <- tryCatch(.in_python(python), error = function(e) "")
  if (!nzchar(py) || !file.exists(py)) return(FALSE)
  code <- paste0("import ", paste(.in_py_modules, collapse = ", "))
  st <- suppressWarnings(system2(py, c("-c", shQuote(code)), stdout = FALSE, stderr = FALSE))
  identical(as.integer(st), 0L)
}

.in_insect_words <- c("insect", "insects", "beetle", "ladybird", "ladybug", "weevil", "bee",
                      "bumblebee", "wasp", "hornet", "ant", "sawfly", "fly", "hoverfly",
                      "mosquito", "midge", "butterfly", "moth", "caterpillar", "bug",
                      "cicada", "aphid", "dragonfly", "damselfly", "grasshopper",
                      "cricket", "mantis", "lacewing", "caddisfly", "mayfly", "stonefly",
                      "earwig", "cockroach", "termite", "scorpionfly")

#' Process insect photos with the back end
#'
#' Runs the validated photo pipeline on each photo:
#' \enumerate{
#'   \item 'Grounding DINO' (base) with the prompt "insect" on the whole photo
#'     (reduced to 480-800 px) finds the main insect, counts other insects
#'     with a score of 0.30 or more, and flags insects cut by the frame;
#'   \item a square crop with a 15% margin is taken at native resolution
#'     (longer side at most 1024 px);
#'   \item 'Grounding DINO' runs again inside the crop and the best box centred
#'     in the middle 60% is kept (two-stage detection);
#'   \item 'SAM' 2.1-B outlines the insect from that box; two jittered boxes
#'     (0.9 and 1.1 times) give the outline stability, and the stage-1 box gives
#'     the agreement between the two stages;
#'   \item the outline is clipped to the box plus 10%, the largest part is kept
#'     and holes are filled; quality metrics are computed;
#'   \item the photo filter (logistic heads on 'OpenCLIP' and 'DINOv2'
#'     embeddings plus quality metrics) gives the probabilities that the photo is
#'     usable, shows a top view, shows the upper wing side (Lepidoptera), shows
#'     the whole insect, and is sharp;
#'   \item ten core traits (crop at most 1024 px) and 57 extended traits (crop at
#'     most 768 px) are computed.
#' }
#' Results are written to `out_dir` (`traits.csv`, `crops/`, `masks/`,
#' `photos/` for downloaded photos) and read back with [in_read_results()], which
#' applies a decision rule with [in_filter()]. Runs can be resumed: photos
#' already in `traits.csv` are skipped.
#'
#' \strong{Insects only.} The prompt, photo filter and trait catalogue were
#' validated for insects. Other prompts are possible but untested and give a
#' warning.
#'
#' @param photos Photos to process: one photo (file path or URL), a character
#'   vector of paths or URLs, a folder (searched recursively; photos in
#'   sub-folders named after species get that species, see [in_read_folder()]),
#'   or a data frame with a `url` or `path` column (for example from
#'   [in_gbif_photos()], [in_inat_photos()] or [in_read_folder()]). Optional
#'   columns `photo_id`, `species` and `order` are kept; `order` enables the
#'   Lepidoptera wing-side rule (see [in_taxonomy()]).
#' @param out_dir Output folder.
#' @param prompt Detector text prompt. The validated prompt is `"insect"`.
#' @param two_stage Run the second detection inside the crop (validated
#'   default). `FALSE` is about 40% faster but gave a worse outline in about 3%
#'   of validation photos.
#' @param photo_filter Run the photo filter (needed for the default decision
#'   rule).
#' @param sam_weights Path to the 'SAM' 2.1 weights; downloaded on first use.
#'   The default is a file in the user cache folder of \pkg{InsectTrait}.
#' @param limit Optional maximum number of photos (for a test run).
#' @param rule Decision rule applied to the results; see [in_filter()].
#' @param white_balance Colour correction applied to the crop before the traits
#'   are computed (the photo filter always sees the uncorrected crop); see
#'   [in_white_balance()]. The validated default is `"none"`.
#' @param python Optional path to the 'Python' executable; see
#'   [in_use_backend()].
#' @return A `in_results` data frame (see [in_read_results()]).
#' @seealso [in_install()], [in_filter()], [in_contact_sheet()]
#' @examples
#' \dontrun{
#' in_use_backend()
#' ex <- in_example_photos()
#' res <- in_process(ex$photo, out_dir = file.path(tempdir(), "in_run"))
#' summary(res)
#'
#' # photos found on GBIF
#' ph <- in_gbif_photos("Harmonia axyridis", n = 20)
#' res <- in_process(ph, out_dir = "PATH_TO_OUTPUT_FOLDER")
#'
#' # one photo, or a folder with one sub-folder per species
#' res1 <- in_process("PATH_TO_ONE_PHOTO.jpg", out_dir = "PATH_TO_OUTPUT_FOLDER")
#' sp <- in_taxonomy(in_read_folder("PATH_TO_SPECIES_FOLDERS"))
#' res2 <- in_process(sp, out_dir = "PATH_TO_OUTPUT_FOLDER")
#' }
#' @export
in_process <- function(photos, out_dir, prompt = "insect", two_stage = TRUE,
                       photo_filter = TRUE, sam_weights = NULL, limit = NULL,
                       rule = "default", white_balance = c("none", "gray_world", "white_patch",
                                                           "shades_of_gray", "gray_pixels"),
                       python = NULL) {
  white_balance <- match.arg(white_balance)
  .in_check_prompt(prompt)
  items <- .in_items(photos)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  inp <- file.path(out_dir, "input.csv")
  utils::write.csv(items, inp, row.names = FALSE)
  if (is.null(sam_weights)) {
    cache <- tools::R_user_dir("InsectTrait", "cache")
    dir.create(cache, showWarnings = FALSE, recursive = TRUE)
    sam_weights <- file.path(cache, "sam2.1_b.pt")
  }
  argv <- c("--input", normalizePath(inp), "--out", normalizePath(out_dir),
            "--url-col", "path", "--id-col", "photo_id", "--prompt", prompt,
            "--sam-weights", sam_weights)
  if ("order" %in% names(items)) argv <- c(argv, "--group-col", "order")
  if (!two_stage) argv <- c(argv, "--one-stage")
  if (!photo_filter) argv <- c(argv, "--no-filter")
  if (!is.null(limit)) argv <- c(argv, "--limit", as.character(as.integer(limit)))
  if (white_balance != "none") argv <- c(argv, "--white-balance", white_balance)
  .in_run_python("insecttrait.run_photos", argv, python)
  in_read_results(out_dir, rule = if (photo_filter) rule else "qc")
}

#' Read back-end results
#'
#' Reads `traits.csv` written by [in_process()] (or by the command-line tool
#' `python -m insecttrait.run_photos`), adds the columns of `input.csv` when it
#' is present (for example species and occurrence key from [in_gbif_photos()]),
#' and applies a decision rule with [in_filter()].
#'
#' @param x Output folder of [in_process()] or path to a `traits.csv` file.
#' @inheritParams in_filter
#' @return A data frame of class `in_results`, one row per photo, with the
#'   back-end columns, `keep`, `reason` and `view`. The output folder is stored
#'   in `attr(x, "dir")`.
#' @examples
#' f <- system.file("extdata", "example_results.csv", package = "InsectTrait")
#' res <- in_read_results(f)
#' table(res$reason)
#' @export
in_read_results <- function(x, rule = "default") {
  f <- if (dir.exists(x)) file.path(x, "traits.csv") else x
  if (!file.exists(f)) stop("No results file found: ", f, call. = FALSE)
  res <- utils::read.csv(f, stringsAsFactors = FALSE, colClasses = c(photo_id = "character"))
  inp <- file.path(dirname(f), "input.csv")
  if (file.exists(inp)) {
    meta <- utils::read.csv(inp, stringsAsFactors = FALSE, colClasses = c(photo_id = "character"))
    add <- setdiff(names(meta), c(names(res), "path"))
    if (length(add)) res <- cbind(res, meta[match(res$photo_id, meta$photo_id), add, drop = FALSE])
  }
  for (k in intersect(c("mask_ok", "cut_by_frame", "keep"), names(res))) res[[k]] <- .in_lgl(res[[k]])
  if (!"p_usable_any" %in% names(res) && rule %in% c("default", "strict", "top_view")) {
    warning("No photo-filter columns; using rule = \"qc\".", call. = FALSE)
    rule <- "qc"
  }
  res <- in_filter(res, rule = rule)
  rownames(res) <- NULL
  attr(res, "dir") <- dirname(f)
  res
}

#' Extended traits from crops and outlines (back end)
#'
#' Computes the full trait set of the back end (colour classes, granularity,
#' texture, pattern landscape metrics, symmetry and shape) for crops and their
#' outlines, for example to re-measure outlines corrected by hand. Requires the
#' 'Python' back end (only 'NumPy', 'pandas' and 'OpenCV' are used).
#'
#' @param image Paths to crops (JPEG or PNG).
#' @param mask Paths to outline masks (PNG, non-zero = insect), one per crop.
#' @param max_side Longer side (pixels) used for the extended traits; 768 is the
#'   validated value.
#' @inheritParams in_process
#' @return A data frame with one row per crop: `photo_id`, quality metrics and
#'   traits (`status` is `"ok"` or the reason for missing traits).
#' @examples
#' \dontrun{
#' in_use_backend()
#' ex <- in_example_photos()
#' in_extended_traits(ex$crop, ex$mask)
#' }
#' @export
in_extended_traits <- function(image, mask, max_side = 768, python = NULL) {
  if (length(image) != length(mask)) stop("Give one mask per image.", call. = FALSE)
  miss <- c(image, mask)[!file.exists(c(image, mask))]
  if (length(miss)) stop("Files not found: ", paste(miss, collapse = ", "), call. = FALSE)
  tmp <- tempfile("in_ext_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  pairs <- data.frame(photo_id = .in_ids(image), image = normalizePath(image),
                      mask = normalizePath(mask), stringsAsFactors = FALSE)
  utils::write.csv(pairs, file.path(tmp, "pairs.csv"), row.names = FALSE)
  out <- file.path(tmp, "extended.csv")
  .in_run_python("insecttrait.api", c("--pairs", file.path(tmp, "pairs.csv"), "--out", out,
                                      "--max-side", as.character(as.integer(max_side))), python)
  utils::read.csv(out, stringsAsFactors = FALSE, colClasses = c(photo_id = "character"))
}

.in_python <- function(python = NULL) {
  py <- python %||% getOption("InsectTrait.python") %||% Sys.getenv("INSECTTRAIT_PYTHON", "")
  if (!nzchar(py)) py <- reticulate::py_config()$python
  py
}

# Runs a back-end module in a separate Python process (PyTorch is never loaded
# into the R session, which avoids thread-library conflicts with R).
.in_run_python <- function(module, args, python = NULL) {
  py <- .in_python(python)
  if (!file.exists(py)) stop("Python not found: ", py, ". See in_install().", call. = FALSE)
  old <- Sys.getenv("PYTHONPATH", unset = NA)
  pp <- system.file("python", package = "InsectTrait")
  Sys.setenv(PYTHONPATH = if (is.na(old) || !nzchar(old)) pp else
    paste(pp, old, sep = .Platform$path.sep))
  on.exit(if (is.na(old)) Sys.unsetenv("PYTHONPATH") else Sys.setenv(PYTHONPATH = old))
  st <- system2(py, c("-m", module, shQuote(args)))
  if (!identical(as.integer(st), 0L)) {
    stop("The Python back end stopped with status ", st, "; see the messages above.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.in_check_prompt <- function(prompt) {
  words <- tolower(trimws(unlist(strsplit(prompt, "[.,;]"))))
  ok <- length(words) > 0 && all(words %in% .in_insect_words)
  if (!ok) {
    warning("InsectTrait is validated for insects only; prompt '", prompt,
            "' is untested.", call. = FALSE)
  }
  invisible(ok)
}

.in_items <- function(photos) {
  if (is.data.frame(photos)) {
    src <- if ("path" %in% names(photos) && any(!is.na(photos$path))) "path" else "url"
    if (!src %in% names(photos)) stop("`photos` needs a `url` or `path` column.", call. = FALSE)
    items <- photos
    items$path <- photos[[src]]
    if (!"photo_id" %in% names(items)) items$photo_id <- .in_ids(items$path)
    items <- items[!is.na(items$path), , drop = FALSE]
  } else if (is.character(photos) && length(photos) == 1L && dir.exists(photos)) {
    items <- in_read_folder(photos)
  } else if (is.character(photos)) {
    local <- !grepl("^https?://", photos)
    if (any(local & !file.exists(photos))) {
      stop("Files not found: ", paste(photos[local & !file.exists(photos)], collapse = ", "),
           call. = FALSE)
    }
    p <- photos
    p[local] <- normalizePath(p[local])
    items <- data.frame(photo_id = .in_ids(photos), path = p, stringsAsFactors = FALSE)
  } else {
    stop("`photos` must be a folder, file paths, URLs or a data frame.", call. = FALSE)
  }
  if (anyDuplicated(items$photo_id)) stop("`photo_id` values must be unique.", call. = FALSE)
  items$photo_id <- as.character(items$photo_id)
  items[, c("photo_id", "path", setdiff(names(items), c("photo_id", "path"))), drop = FALSE]
}

.in_ids <- function(p) {
  tools::file_path_sans_ext(basename(sub("\\?.*$", "", p)))
}

.in_lgl <- function(v) {
  if (is.logical(v)) return(v)
  out <- toupper(trimws(as.character(v))) %in% c("TRUE", "T", "1")
  out[is.na(v) | trimws(as.character(v)) == ""] <- NA
  out
}
