#' InsectTrait: colour, pattern and shape traits of insects from photographs
#'
#' @description
#' \pkg{InsectTrait} measures colour, pattern and shape traits of insects in
#' field photographs, for example the research-grade 'iNaturalist' photos
#' served by 'GBIF'. \strong{It is for insects only.} The detector prompt, the
#' photo filter and the trait catalogue were developed and validated on 780
#' photos of 48 insect species (Coleoptera, Diptera, Hymenoptera and
#' Lepidoptera). The package should not be used for birds, reptiles, plants or
#' other organisms without a new validation.
#'
#' @section Workflow:
#' \enumerate{
#'   \item Find photos: [in_gbif_photos()], [in_inat_photos()] and
#'     [in_download_photos()], or use your own: one photo, a folder, or a
#'     folder with one sub-folder per species ([in_read_folder()],
#'     [in_taxonomy()]).
#'   \item Process photos with the optional 'Python' back end
#'     ([in_install()], [in_use_backend()], [in_process()]):
#'     two-pass 'Grounding DINO' detection, 'SAM' 2.1 outline, quality control,
#'     photo filter and 67 traits.
#'   \item Keep usable photos with a validated decision rule: [in_filter()].
#'   \item Check photos and traits: [in_colour_check()], [in_white_balance()],
#'     [in_augment()], [in_augment_check()], [in_trait_info()],
#'     [in_reliable_traits()], [in_repeatability()], [in_invariance()].
#'   \item Summarise and plot: [in_summarise()], [in_plot_traits()],
#'     [in_contact_sheet()].
#' }
#' Core colour traits and outline quality metrics can be computed in R alone
#' from a crop and its outline with [in_traits()].
#'
#' @section Data:
#' [in_catalogue] (reliability of the 67 traits), [in_validation] (the 780
#' validation photos) and [in_benchmark] (detector and segmenter benchmark).
#'
#' @keywords internal
"_PACKAGE"

.in_core_traits <- c("L_mean", "L_sd", "C_mean", "hue_deg", "hue_conc", "vap_wp",
                     "darkness_cw", "dark_frac_otsu", "L_entropy")
.in_circular <- c("hue_deg", "dominant_hue")

utils::globalVariables(c("in_catalogue", "in_validation", "in_benchmark"))
