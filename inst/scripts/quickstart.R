# Quick start: runs as-is with the files shipped in the package (no Python).
library(InsectTrait)

ex <- in_example_photos()
ex[, c("photo_id", "species", "order", "rights_holder", "license")]

# Core colour traits and outline quality from the shipped crops and outlines
tr <- in_traits(ex$crop, ex$mask)
tr[, c("photo_id", "mask_ok", "D_px", "L_mean", "C_mean", "dark_frac_otsu")]

# Back-end results with the default decision rule
res <- in_example_results()
summary(res)
in_contact_sheet(res, ncol = 4)

# Trait reliability
table(in_catalogue$family, in_catalogue$verdict)
in_trait_info(verdict = c("use with care", "not reliable"))

# Repeatability on the validation photos (two photos per insect)
v <- in_filter(in_validation, "default")
in_repeatability(v[v$keep, ], traits = c("L_mean", "C_mean", "dark_frac_otsu"), boot = 100)
