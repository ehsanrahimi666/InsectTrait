test_that("R traits match the Python back end on the example crops", {
  ref <- read.csv(test_path("fixtures", "python_core_traits.csv"),
                  colClasses = c(photo_id = "character"))
  ex <- in_example_photos()
  i <- match(ref$photo_id, ex$photo_id)
  tr <- in_traits(ex$crop[i], ex$mask[i])
  expect_equal(tr$photo_id, ref$photo_id)
  for (k in c("px", "border_touch", "clip_hi", "clip_lo", "D_px", "dark_frac_otsu")) {
    expect_equal(tr[[k]], ref[[k]], tolerance = 1e-10, info = k)
  }
  for (k in c("mask_frac", "solidity", "sharp", "L_mean", "L_sd", "C_mean", "hue_deg",
              "hue_conc", "vap_wp", "darkness_cw", "L_entropy")) {
    expect_equal(tr[[k]], ref[[k]], tolerance = 1e-4, info = k)
  }
})

test_that("traits behave as expected on a synthetic two-tone disk", {
  img <- array(150, c(120, 120, 3))
  yy <- row(img[, , 1])
  xx <- col(img[, , 1])
  disk <- (yy - 60)^2 + (xx - 60)^2 < 45^2
  img[, , 1][disk] <- 220
  img[, , 2][disk] <- 120
  img[, , 3][disk] <- 30
  left <- disk & xx < 60
  img[, , 1][left] <- 30
  img[, , 2][left] <- 25
  img[, , 3][left] <- 20
  t <- in_traits(img, disk)
  expect_equal(t$px, sum(disk))
  expect_true(t$mask_ok)
  expect_equal(t$border_touch, 0)
  expect_gt(t$solidity, 0.95)
  expect_equal(t$dark_frac_otsu, mean(xx[disk] < 60), tolerance = 1e-12)
  expect_gt(t$C_mean, 10)
  expect_equal(t$D_px, round(2 * sqrt(sum(disk) / pi), 1))
  small <- in_traits(img, disk & yy < 52 & yy > 48)
  expect_true(is.na(small$L_mean))
  expect_false(small$mask_ok)
})

test_that("CIELAB conversion gives reference values", {
  lab <- in_rgb_to_lab(rbind(c(255, 255, 255), c(0, 0, 0), c(255, 0, 0)))
  expect_equal(unname(lab[1, ]), c(100, 0, 0), tolerance = 1e-3)
  expect_equal(unname(lab[2, ]), c(0, 0, 0), tolerance = 1e-6)
  expect_equal(unname(lab[3, 1]), 53.24, tolerance = 1e-3)
  ex <- in_example_photos()
  a <- in_rgb_to_lab(in_read_image(ex$crop[1]))
  expect_equal(dim(a)[3], 3L)
})

test_that("input errors are informative", {
  expect_error(in_traits(array(100, c(5, 5, 3)), matrix(TRUE, 4, 4)), "same height")
  expect_error(in_traits(matrix(1, 5, 5), matrix(TRUE, 5, 5)), "array")
  expect_error(in_traits(c("a.jpg", "b.jpg"), "a.png"), "one mask")
  expect_warning(in_traits(array(0.5, c(30, 30, 3)), matrix(TRUE, 30, 30)), "0-1")
})
