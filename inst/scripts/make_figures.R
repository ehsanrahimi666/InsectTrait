# Figures of the InsectTrait paper, made from data shipped with the package.
# Usage: Rscript make_figures.R OUTPUT_FOLDER
# All photos are the seven CC0 / CC BY example photos (see in_example_photos()).
library(InsectTrait)

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else "figures"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

col_dark <- "#1F3B66"; col_mid <- "#4A6FA5"; col_light <- "#DCE9F5"
col_keep <- "#2E8B57"; col_rej <- "#C0504D"; col_warn <- "#E69F00"
fam_col <- c(colour = "#C0504D", pattern = "#4A6FA5", shape = "#2E8B57")
verdict_col <- c("reliable" = "#2E8B57", "use with care" = "#E69F00", "not reliable" = "#C0504D")

png_fig <- function(name, w, h) {
  png(file.path(out, name), width = w, height = h, units = "in", res = 300, type = "cairo")
  par(family = "sans", cex = 0.8)
}
panel_label <- function(x, cex = 1.1, line = 0.3) {
  mtext(x, side = 3, adj = 0, line = line, font = 2, cex = cex)
}
draw_img <- function(img, main = NULL) {
  h <- dim(img)[1]; w <- dim(img)[2]
  plot.new()
  plot.window(c(0, w), c(0, h), asp = 1, xaxs = "i", yaxs = "i")
  rasterImage(as.raster(img / 255), 0, 0, w, h)
  if (!is.null(main)) title(main, cex.main = 0.85, font.main = 1, line = 0.3)
  invisible(c(h = h, w = w))
}
box_px <- function(h, x1, y1, x2, y2, ...) rect(x1, h - y2, x2, h - y1, ...)
outline <- function(m, col, lwd = 2) {
  h <- nrow(m); w <- ncol(m)
  z <- matrix(0, w + 2, h + 2)
  z[2:(w + 1), 2:(h + 1)] <- t(m[h:1, , drop = FALSE])
  for (k in contourLines((0:(w + 1)) - 0.5, (0:(h + 1)) - 0.5, z, levels = 0.5)) {
    lines(k$x, k$y, col = col, lwd = lwd)
  }
}

ex <- in_example_photos()
res <- in_example_results()
res <- res[match(ex$photo_id, res$photo_id), ]

# ---- Figure 1: workflow ----------------------------------------------------
png_fig("fig1_workflow.png", 7, 4.4)
layout(matrix(c(1, 2, 3, 4, 5, 5, 5, 5), 2, byrow = TRUE), heights = c(1.25, 0.62))
par(mar = c(0.4, 0.4, 2.2, 0.4))
i <- 1
r <- res[i, ]
photo <- in_read_image(ex$photo[i]); crop <- in_read_image(ex$crop[i]); m <- in_read_mask(ex$mask[i])
d <- draw_img(photo, "Whole photo and square crop")
box_px(d["h"], r$crop_x0, r$crop_y0, r$crop_x1, r$crop_y1, border = "#FFD400", lwd = 2, lty = 2)
panel_label("a")
d <- draw_img(crop, "Square crop: stage-2 detection")
box_px(d["h"], r$x1, r$y1, r$x2, r$y2, border = "#FFD400", lwd = 2)
panel_label("b")
draw_img(crop, "SAM 2.1 outline")
outline(m, "#00E676", 2)
panel_label("c")
par(mar = c(4, 6.2, 2.2, 0.8))
p <- c(usable = r$p_usable_any, "top view" = r$p_top_view, "whole insect" = r$p_fully_visible,
       sharp = r$p_sharp)
bp <- barplot(rev(p), horiz = TRUE, las = 1, xlim = c(0, 1), col = col_mid, border = NA,
              xlab = "Probability", main = "Photo filter", cex.main = 0.85, font.main = 1)
abline(v = 0.5, lty = 2, col = "grey40")
panel_label("d")
par(mar = c(0.3, 0.5, 1.6, 0.5))
plot.new(); plot.window(c(0, 100), c(0, 40))
steps <- c("1 Photo, folder,\nspecies folders,\nGBIF, iNat", "2 Grounding\nDINO, whole\nphoto", "3 Square crop\n(native\nresolution)",
           "4 Grounding\nDINO in\nthe crop", "5 SAM 2.1-B\noutline +\nstability", "6 Outline QC\n+ photo\nfilter",
           "7 Decision\nrule", "8 67 traits\n+ catalogue\nverdict")
fill <- c(col_light, rep("#E8EEF7", 4), "#FDEBD3", "#FDEBD3", "#DFF2E6")
xs <- seq(6.2, 93.8, length.out = length(steps))
for (k in seq_along(steps)) {
  rect(xs[k] - 5.6, 19, xs[k] + 5.6, 39, col = fill[k], border = col_dark, lwd = 0.8)
  text(xs[k], 29, steps[k], cex = 0.66)
  if (k < length(steps)) arrows(xs[k] + 5.7, 29, xs[k + 1] - 5.7, 29, length = 0.05, col = col_dark)
}
text(50, 11, "Steps 2-6 and all traits: Python back end (in_process), run as a separate process",
     cex = 0.68, col = col_dark)
text(50, 3, "In R alone: step 7 (in_filter), core traits and outline QC (in_traits), colour checks, augmentation, reliability",
     cex = 0.68, col = col_keep)
panel_label("e")
dev.off()

# ---- Figure 2: detector and segmenter benchmark ----------------------------
b <- in_benchmark
png_fig("fig2_benchmark.png", 7, 3.5)
layout(matrix(1:2, 1), widths = c(1.2, 1.4))
par(mar = c(4.2, 6.8, 3.2, 0.8))
det <- b$detectors[b$detectors$detector != "none", ]
lab <- c(gdino_base = "Grounding DINO-B", gdino_tiny = "Grounding DINO-T", owlv2_base = "OWLv2-B",
         owlvit_base = "OWL-ViT-B", yoloworld_x = "YOLO-World-X")
M <- tapply(det$found, list(det$prompt, det$detector), mean)[, rev(names(lab))]
barplot(M * 100, beside = TRUE, horiz = TRUE, col = c(col_mid, col_warn), border = NA, xlim = c(0, 106),
        names.arg = lab[colnames(M)], cex.names = 0.72, las = 1, xlab = "Crops with a detection (%)",
        axes = FALSE)
axis(1, at = seq(0, 100, 20))
legend(x = 106, y = par("usr")[4], c("\"insect\"", "six group names"), fill = c(col_mid, col_warn),
       border = NA, bty = "n", cex = 0.66, ncol = 2, xjust = 1, yjust = 0, xpd = TRUE)
panel_label("a", line = 1.7)
par(mar = c(4.2, 4.2, 3.2, 7.6))
cmb <- b$combinations
segs <- c("sam2.1_b", "sam2.1_l", "sam_b", "hf_sam_base", "mobile_sam", "fastsam_x", "grabcut")
pal <- setNames(c("#C0504D", "#1F3B66", "#4A6FA5", "#9DB9E0", "#E69F00", "#8E6BB0", "grey45"), segs)
plot(cmb$sec_total, cmb$iou_consensus, log = "x", pch = 16, col = adjustcolor(pal[cmb$segmenter], 0.85),
     xlab = "Seconds per crop (T4 GPU, log scale)", ylab = "Mean IoU with consensus outline",
     ylim = c(0.15, 1), las = 1, cex = 0.9)
sel <- cmb$detector == "gdino_base" & cmb$prompt == "generic" & cmb$segmenter == "sam2.1_b"
points(cmb$sec_total[sel], cmb$iou_consensus[sel], pch = 1, cex = 2.2, lwd = 1.5)
lg <- legend(x = 10^par("usr")[2], y = par("usr")[4],
             c("SAM 2.1-B", "SAM 2.1-L", "SAM-B", "SAM-B (HF)", "MobileSAM", "FastSAM-X", "GrabCut"),
             pch = 16, col = pal, bty = "n", cex = 0.7, xpd = TRUE, title = "Segmenter", title.adj = 0.1)
legend(x = 10^par("usr")[2], y = lg$rect$top - lg$rect$h, "used in\nInsectTrait", pch = 1, pt.cex = 1.8,
       pt.lwd = 1.5, bty = "n", cex = 0.7, xpd = TRUE)
panel_label("b", line = 1.7)
dev.off()

# ---- Figure 3: outline quality, decision rules and photo filter ------------
v <- in_validation
bench <- v[v$role == "benchmark", ]
rules <- c("none", "qc", "default", "strict", "top_view")
rule_lab <- c("no rule", "outline QC", "default", "strict", "top view")
st <- t(sapply(rules, function(rl) {
  k <- in_filter(bench, rl)$keep
  c(kept = 100 * mean(k), wrong = 100 * mean(bench$outline_rating[k] == "wrong"),
    minor = 100 * mean(bench$outline_rating[k] == "minor error"))
}))
png_fig("fig3_filter.png", 7, 5.2)
par(mfrow = c(2, 2), mar = c(4, 4.4, 1.8, 0.8))
bp <- barplot(st[, "kept"], names.arg = rule_lab, col = c("grey70", "grey55", col_mid, col_dark, col_keep),
              border = NA, ylim = c(0, 110), las = 1, ylab = "Benchmark photos kept (%)", cex.names = 0.75)
text(bp, st[, "kept"] + 5, sprintf("%.0f", st[, "kept"]), cex = 0.75)
panel_label("a")
bp <- barplot(t(st[, c("wrong", "minor")]), names.arg = rule_lab, col = c(col_rej, "#F2B8B5"), border = NA,
              ylim = c(0, 25), las = 1, ylab = "Outline errors among kept (%)", cex.names = 0.75)
text(bp, st[, "wrong"] + st[, "minor"] + 1.3, sprintf("%.1f", st[, "wrong"]), cex = 0.7, col = col_rej)
legend("topright", c("wrong", "minor error"), fill = c(col_rej, "#F2B8B5"), border = NA, bty = "n", cex = 0.75)
panel_label("b")
pf <- in_benchmark$photo_filter
qs <- c(upperside_lep = "Upper wing side\n(Lepidoptera)", sharp = "Sharp", top_view = "Top view",
        usable_any = "Usable", fully_visible = "Whole insect")
get_auc <- function(q, f) pf$auc[pf$question == q & f(pf)]
auc <- data.frame(q = unname(qs),
                  best = sapply(names(qs), get_auc, f = function(d) d$used),
                  qc = sapply(names(qs), get_auc, f = function(d) d$features == "QC+shape"),
                  zs = sapply(names(qs), get_auc, f = function(d) d$features == "zero-shot CLIP (full crop, no training)"))
par(mar = c(4, 7.2, 1.8, 0.8))
y <- rev(seq_len(nrow(auc)))
plot(NA, xlim = c(0.5, 1), ylim = c(0.5, nrow(auc) + 1.3), yaxt = "n", xlab = "Cross-validated AUC", ylab = "")
axis(2, at = y, labels = auc$q, las = 1, cex.axis = 0.72)
abline(h = y, col = "grey92")
points(auc$zs, y, pch = 17, col = col_warn)
points(auc$qc, y, pch = 15, col = "grey50")
points(auc$best, y, pch = 19, col = col_dark, cex = 1.2)
legend("top", c("shipped head", "QC features", "zero-shot"), horiz = TRUE,
       pch = c(19, 15, 17), col = c(col_dark, "grey50", col_warn), bty = "n", cex = 0.66)
panel_label("c")
par(mar = c(4, 4.4, 1.8, 0.8))
br <- seq(0, 1, 0.05)
h1 <- hist(bench$p_usable_any[bench$label_usable], breaks = br, plot = FALSE)
h0 <- hist(bench$p_usable_any[!bench$label_usable], breaks = br, plot = FALSE)
plot(h1, col = adjustcolor(col_keep, 0.6), border = NA, main = "", xlab = "p_usable_any",
     ylab = "Benchmark photos", ylim = c(0, max(h1$counts, h0$counts) * 1.1), las = 1)
plot(h0, col = adjustcolor(col_rej, 0.6), border = NA, add = TRUE)
abline(v = 0.5, lty = 2)
legend("topleft", c("labelled usable", "labelled not usable"), fill = adjustcolor(c(col_keep, col_rej), 0.6),
       border = NA, bty = "n", cex = 0.72)
panel_label("d")
dev.off()

# ---- Figure 4: trait reliability --------------------------------------------
cat <- in_catalogue
png_fig("fig4_reliability.png", 7, 5.4)
layout(matrix(c(1, 2, 3, 3), 2, byrow = TRUE))
par(mar = c(4.2, 4.4, 1.8, 0.8))
lv <- c(all = "all pairs", qc = "QC +\nstability", default = "default", strict = "strict", top = "top view")
med <- sapply(names(lv), function(k) tapply(cat[[paste0("icc_", k)]], cat$family, median, na.rm = TRUE))
npairs <- sapply(names(lv), function(k) cat[[paste0("n_", k)]][1])
matplot(t(med), type = "b", pch = 19, lty = 1, col = fam_col[rownames(med)], xaxt = "n", las = 1,
        ylim = c(0.5, 0.95), xlab = "", ylab = "Median photo-to-photo ICC")
axis(1, at = seq_along(lv), labels = lv, cex.axis = 0.7, padj = 0.5)
mtext(paste0("n = ", npairs), side = 1, at = seq_along(lv), line = 2.6, cex = 0.5, col = "grey35")
legend("bottomright", paste0(rownames(med), " (", table(cat$family)[rownames(med)], ")"),
       col = fam_col[rownames(med)], pch = 19, lty = 1, bty = "n", cex = 0.75)
panel_label("a")
plot(cat$icc_strict, cat$inv_photometric_min, pch = 21, bg = verdict_col[as.character(cat$verdict)],
     col = "white", cex = 1.3, xlim = c(0.3, 1), ylim = c(0, 1.02), las = 1,
     xlab = "Repeatability (ICC, strict rule)", ylab = "Photometric invariance (min. ICC)")
abline(v = c(0.4, 0.6), h = c(0.7, 0.8), lty = c(3, 2, 3, 2), col = "grey50")
nr <- which(cat$verdict == "not reliable")
pos <- c(hue_deg = 4, dominant_hue = 4, glcm_energy = 2, sym_pattern_r = 1)[cat$trait[nr]]
text(cat$icc_strict[nr], cat$inv_photometric_min[nr], cat$trait[nr], pos = pos, cex = 0.62)
legend(0.3, 0.58, names(verdict_col), pt.bg = verdict_col, pch = 21, col = "white", pt.cex = 1.3, bty = "n",
       cex = 0.72, title = "Verdict (54 / 9 / 4)", title.adj = 0)
panel_label("b")
par(mar = c(4.2, 4.4, 1.8, 9))
sel <- c("L_mean", "C_mean", "dark_frac_otsu", "colour_shannon", "gran_D64", "n_colour_classes",
         "dominant_C", "land_n_patches", "land_edge_density")
D <- c(32, 64, 128, 256)
Y <- t(as.matrix(cat[match(sel, cat$trait), paste0("inv_D", D)]))
cl <- c("#1F3B66", "#4A6FA5", "#9DB9E0", "#2E8B57", "#7FC29B", "#E69F00", "#C0504D", "#8E6BB0", "grey40")
matplot(D, Y, type = "b", log = "x", pch = 19, lty = 1, col = cl, xaxt = "n", las = 1,
        ylim = c(floor(10 * min(Y, na.rm = TRUE)) / 10, 1),
        xlab = "Insect diameter after downscaling (px)", ylab = "ICC with full resolution")
axis(1, at = D)
abline(h = 0.8, lty = 2, col = "grey50")
legend("right", sel, col = cl, pch = 19, lty = 1, bty = "n", cex = 0.7, inset = c(-0.3, 0), xpd = TRUE)
panel_label("c")
dev.off()

# ---- Figure 5: colour checks, augmentation and colour correction -----------
png_fig("fig5_colour.png", 7, 6.3)
layout(matrix(c(1:8, rep(9, 4), rep(10, 4)), 2, byrow = TRUE), heights = c(1.2, 5.1))
par(mar = c(0.3, 0.3, 2.2, 0.3))
k <- which(ex$species == "Aglais io")
img <- in_read_image(ex$crop[k]); m <- in_read_mask(ex$mask[k])
aug_show <- list("Original" = list(image = img, mask = m),
                 "Quarter turn" = in_augment(img, m, "rotate90", value = 1),
                 "Mirror" = in_augment(img, m, "flip", value = "horizontal"),
                 "Brightness 1.25" = in_augment(img, m, "brightness", value = 1.25),
                 "Contrast 0.75" = in_augment(img, m, "contrast", value = 0.75),
                 "Saturation 1.25" = in_augment(img, m, "saturation", value = 1.25),
                 "Hue +0.05" = in_augment(img, m, "hue", value = 0.05),
                 "White balance" = in_augment(img, m, "white_balance", value = c(1.2, 0.8)))
for (j in seq_along(aug_show)) {
  draw_img(aug_show[[j]]$image, names(aug_show)[j])
  outline(aug_show[[j]]$mask, "#00E676", 1)
  if (j == 1) panel_label("a")
}
types <- c("rotate90", "flip", "rotate", "brightness", "contrast", "saturation", "hue",
           "white_balance", "exposure", "jpeg", "blur")
tlab <- c("Quarter turn", "Mirror", "Rotation (up to 30\u00b0)", "Brightness", "Contrast", "Saturation", "Hue",
          "White balance", "Exposure", "JPEG", "Blur")
core <- c("L_mean", "L_sd", "C_mean", "hue_deg", "hue_conc", "vap_wp", "darkness_cw", "dark_frac_otsu",
          "L_entropy")
ag <- in_benchmark$augmentation
M <- xtabs(median_rel_change ~ type + trait, ag)[types, core]
z <- t(M[nrow(M):1, ])
par(mar = c(6.2, 8.2, 3.2, 0.6))
brk <- c(0, 0.05, 0.1, 0.25, 0.5, 1, max(1.01, max(z) + 0.01))
pal <- c("#F7FBFF", "#DEEBF7", "#FDD49E", "#FC8D59", "#D7301F", "#7F0000")
image(seq_len(nrow(z)), seq_len(ncol(z)), z, breaks = brk, col = pal, axes = FALSE, xlab = "", ylab = "")
text(row(z), col(z), sprintf("%.2f", z), cex = 0.55, col = ifelse(z > 0.5, "white", "black"))
axis(1, at = seq_len(nrow(z)), labels = core, las = 2, cex.axis = 0.72, tick = FALSE, line = -0.6)
axis(2, at = seq_len(ncol(z)), labels = rev(tlab), las = 1, cex.axis = 0.72, tick = FALSE, line = -0.6)
abline(h = c(4.5, 8.5), col = "grey30", lwd = 1)
box(col = "grey30")
mtext("Median change / between-photo SD", side = 3, line = 0.3, cex = 0.7)
panel_label("b", line = 1.6)
wb <- in_benchmark$white_balance
ms <- c("gray_world", "white_patch", "shades_of_gray", "gray_pixels", "gray_pixels_if_cast")
mlab <- c("Grey world", "White patch", "Shades of grey", "Grey pixels", "Grey pixels if cast > 0.2")
mcol <- c("#C0504D", "#E69F00", "#8E6BB0", "#2E8B57", "#1F3B66")
base <- wb[wb$method == "none", ]
ylab <- sprintf("%s (%.2f)", core, base$icc[match(core, base$trait)])
off <- seq(0.3, -0.3, length.out = length(ms))
nt <- length(core)
par(mar = c(6.2, 8.4, 3.2, 0.8))
xl <- range(c(wb$lower, wb$upper, 0), na.rm = TRUE)
plot(NA, xlim = xl, ylim = c(0.5, nt + 0.5), yaxt = "n", xlab = "", ylab = "", las = 1, cex.axis = 0.75)
title(xlab = "ICC difference from no correction", line = 2.2, cex.lab = 0.85)
abline(v = 0, lty = 2, col = "grey40")
abline(h = seq(1.5, nt - 0.5), col = "grey88")
for (j in seq_along(ms)) {
  d <- wb[wb$method == ms[j], ]
  d <- d[match(core, d$trait), ]
  y <- nt:1 + off[j]
  segments(d$lower, y, d$upper, y, col = mcol[j], lwd = 1.2)
  points(d$diff, y, pch = 19, cex = 0.55, col = mcol[j])
}
axis(2, at = nt:1, labels = ylab, las = 1, cex.axis = 0.72, tick = FALSE, line = -0.6)
legend(mean(par("usr")[1:2]), grconvertY(0.005, "ndc", "user"), mlab, col = mcol, pch = 19, lwd = 1.2,
       bty = "n", cex = 0.68, ncol = 2, xjust = 0.5, yjust = 0, xpd = NA)
mtext(sprintf("Repeatability, %d pairs (ICC without correction)", base$n_pairs[1]), side = 3, line = 0.3,
      cex = 0.7)
panel_label("c", line = 1.6)
dev.off()

# ---- Figure 6: worked example ----------------------------------------------
png_fig("fig6_example.png", 7, 6.2)
layout(matrix(c(1:8, rep(9, 4)), 3, byrow = TRUE), heights = c(1, 1, 1.25))
par(mar = c(0.3, 0.3, 2.4, 0.3))
for (k in seq_len(nrow(ex))) {
  crop <- in_read_image(ex$crop[k]); m <- in_read_mask(ex$mask[k])
  draw_img(crop)
  outline(m, if (res$keep[k]) "#00E676" else "#FF1744", 2)
  title(ex$species[k], font.main = 3, cex.main = 0.8, line = 1.1)
  title(res$reason[k], font.main = 1, cex.main = 0.7, line = 0.25,
        col.main = if (res$keep[k]) col_keep else col_rej)
  if (k == 1) panel_label("a")
}
plot.new()
legend("center", c("kept (default rule)", "rejected"), col = c("#00E676", "#FF1744"), lwd = 2.5, bty = "n",
       cex = 0.9)
par(mar = c(4.2, 10.5, 2, 1))
vv <- in_filter(in_validation, "default")
vv <- vv[vv$keep & vv$view %in% "top" & vv$order == "Lepidoptera", ]
nsp <- table(vv$species)
vv <- vv[vv$species %in% names(nsp[nsp >= 3]), ]
spm <- sort(tapply(vv$dark_frac_otsu, vv$species, median))
vv$species <- factor(vv$species, levels = names(spm))
boxplot(dark_frac_otsu ~ species, data = vv, horizontal = TRUE, las = 1, outline = FALSE, col = col_light,
        border = col_mid, xlab = "Dark fraction of the insect (dark_frac_otsu)", ylab = "",
        cex.axis = 0.78, ylim = c(0, 1), yaxt = "n")
axis(2, at = seq_along(spm), labels = paste0(names(spm), " (", nsp[names(spm)], ")"), las = 1,
     cex.axis = 0.78, font = 3)
stripchart(dark_frac_otsu ~ species, data = vv, method = "jitter", jitter = 0.15, pch = 16, cex = 0.55,
           col = adjustcolor(col_dark, 0.7), add = TRUE)
panel_label("b")
dev.off()

message("Figures written to ", normalizePath(out))
