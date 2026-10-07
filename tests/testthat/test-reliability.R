test_that("ICC(1) matches a hand calculation", {
  v <- c(1, 2, 2, 3, 5, 6, 8, 9)
  g <- rep(1:4, each = 2)
  m <- tapply(v, g, mean)
  msb <- 2 * sum((m - mean(v))^2) / 3
  msw <- sum((v - m[g])^2) / 4
  expect_equal(in_icc(v, g), (msb - msw) / (msb + msw))
  expect_true(is.na(in_icc(c(1, 2, 3, 4), c(1, 1, 2, 2))))
  expect_true(is.na(in_icc(rep(1, 6), rep(1:3, each = 2))))
  expect_equal(in_icc(c(v, NA, Inf, 4), c(g, 5, 5, 6)), in_icc(v, g))
})

test_that("repeatability reproduces the catalogue", {
  v <- in_filter(in_validation, "default")
  r <- in_repeatability(v[v$keep, ], traits = c("L_mean", "C_mean", "hue_deg", "dark_frac_otsu"),
                        boot = 20)
  cat <- in_catalogue[match(r$trait, in_catalogue$trait), ]
  expect_equal(r$icc, cat$icc_default, tolerance = 2e-3)
  expect_equal(r$n_groups, cat$n_default)
  expect_true(all(r$lower <= r$icc & r$icc <= r$upper))
  expect_error(in_repeatability(v, group = "nope"), "not found")
  expect_error(in_repeatability(v, traits = "nope"), "not found")
})

test_that("bootstrap does not change the user's random stream", {
  set.seed(10)
  a <- runif(1)
  set.seed(10)
  v <- in_filter(in_validation, "default")
  invisible(in_repeatability(v[v$keep, ], traits = "L_mean", boot = 5))
  expect_equal(runif(1), a)
})

test_that("perturbations keep sizes and behave as expected", {
  ex <- in_example_photos()
  img <- in_read_image(ex$crop[1])
  m <- in_read_mask(ex$mask[1])
  for (tp in c("white_balance", "exposure", "jpeg", "blur", "rotate", "perspective", "rotate90",
               "flip", "brightness", "contrast", "saturation", "hue")) {
    p <- in_augment(img, m, tp)
    expect_equal(dim(p$image), dim(img), info = tp)
    expect_equal(dim(p$mask), dim(m), info = tp)
    expect_true(all(p$image >= 0 & p$image <= 255), info = tp)
  }
  same <- in_augment(img, m, "exposure", value = 1)
  expect_lte(max(abs(same$image - img)), 1)
  r0 <- in_augment(img, m, "rotate", value = 0)
  expect_equal(r0$mask, m)
  dark <- in_augment(img, m, "exposure", value = 0.6)
  expect_lt(in_traits(dark$image, m)$L_mean, in_traits(img, m)$L_mean)
})

test_that("invariance study returns ICCs per trait and change", {
  ex <- in_example_photos()
  inv <- in_invariance(ex$crop[1:4], ex$mask[1:4], types = c("exposure", "jpeg"),
                       rungs = 64, min_n = 3)
  expect_equal(rownames(inv), c("L_mean", "L_sd", "C_mean", "hue_deg", "hue_conc", "vap_wp",
                                "darkness_cw", "dark_frac_otsu", "L_entropy"))
  expect_true(all(c("exposure", "jpeg", "D64") %in% names(inv)))
  expect_gt(inv["L_mean", "jpeg"], 0.9)
  expect_equal(nrow(attr(inv, "data")), 16)  # 4 photos x (original, 2 changes, D64)
})
