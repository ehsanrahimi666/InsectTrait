test_that("catalogue has the validated verdicts", {
  expect_equal(nrow(in_catalogue), 67)
  expect_equal(as.vector(table(in_catalogue$verdict)), c(54, 9, 4))
  expect_equal(as.vector(table(in_catalogue$family)[c("colour", "pattern", "shape")]), c(37, 25, 5))
  rel <- in_catalogue$verdict == "reliable"
  expect_true(all(in_catalogue$icc_strict[rel] >= 0.6))
  expect_true(all(in_catalogue$inv_photometric_min[rel] >= 0.8))
  expect_true(all(in_catalogue$min_D_px[rel] <= 128))
  expect_true(all(in_catalogue$trait %in% names(in_validation)))
})

test_that("trait lookups work", {
  expect_equal(in_trait_info("hue_deg")$verdict, factor("not reliable", levels(in_catalogue$verdict)))
  expect_equal(nrow(in_trait_info(verdict = "not reliable")), 4)
  expect_error(in_trait_info("nope"), "Unknown")
  expect_length(in_reliable_traits(), 54)
  expect_false("hue_deg" %in% in_reliable_traits())
  expect_true(all(in_catalogue$min_D_px[match(in_reliable_traits(insect_px = 64),
                                              in_catalogue$trait)] <= 64))
  expect_setequal(in_reliable_traits(family = "shape"),
                  c("sym_mask_iou", "shape_circularity", "shape_solidity", "shape_aspect",
                    "shape_roughness"))
})

test_that("benchmark tables are complete", {
  expect_named(in_benchmark, c("detectors", "combinations", "photo_filter", "white_balance",
                               "augmentation"))
  wb <- in_benchmark$white_balance
  expect_equal(nrow(wb), 54)
  expect_true(all(wb$diff[wb$method == "none"] == 0))
  expect_true(all(wb$icc > 0 & wb$icc <= 1))
  expect_equal(nrow(in_benchmark$augmentation), 99)
  expect_true(all(in_benchmark$augmentation$median_rel_change >= 0))
  pf <- in_benchmark$photo_filter
  expect_equal(sum(pf$used), 5)
  expect_equal(pf$auc[pf$used & pf$question == "upperside_lep"], 0.944)
  expect_equal(nrow(in_benchmark$detectors), 11)
  expect_equal(nrow(in_benchmark$combinations), 77)
  expect_true(all(in_benchmark$combinations$n == 480))
})
