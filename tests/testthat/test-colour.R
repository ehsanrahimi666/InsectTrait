grey_scene <- function(cast = c(1.08, 1, 0.92)) {
  img <- array(0, c(60, 60, 3))
  for (ch in 1:3) img[, , ch] <- round(pmin(255, 120 * cast[ch]))
  img[20:40, 20:40, 1] <- 200
  img[20:40, 20:40, 2] <- 60
  img[20:40, 20:40, 3] <- 30
  m <- matrix(FALSE, 60, 60)
  m[20:40, 20:40] <- TRUE
  list(img = img, mask = m)
}

test_that("colour check finds a cast on neutral background pixels", {
  s <- grey_scene()
  ck <- in_colour_check(s$img, s$mask)
  expect_gt(ck$cast, 0.2)
  expect_gt(ck$cast_rg, 0)
  expect_match(ck$issues, "colour cast")
  ok <- in_colour_check(grey_scene(c(1, 1, 1))$img, s$mask)
  expect_lt(ok$cast, 0.01)
  expect_equal(ok$issues, "ok")
  green <- array(0, c(30, 30, 3)); green[, , 2] <- 200
  expect_true(is.na(in_colour_check(green)$cast))
  ex <- in_example_photos()
  expect_equal(nrow(in_colour_check(ex$crop[1:2], ex$mask[1:2])), 2)
})

test_that("white balance removes a global cast", {
  s <- grey_scene()
  for (meth in c("gray_world", "white_patch", "shades_of_gray", "gray_pixels")) {
    wb <- in_white_balance(s$img, meth, mask = s$mask, use = "background")
    bgv <- apply(wb, 3, function(ch) mean(ch[!s$mask]))
    expect_lt(diff(range(bgv)), 3, label = meth)
  }
  green <- array(0, c(30, 30, 3)); green[, , 2] <- 200
  expect_identical(in_white_balance(green, "gray_pixels"), green)
  ex <- in_example_photos()
  a <- in_traits(ex$crop[1], ex$mask[1])
  b <- in_traits(ex$crop[1], ex$mask[1], white_balance = "gray_world")
  expect_equal(a$px, b$px)
  expect_false(isTRUE(all.equal(a$C_mean, b$C_mean)))
})

test_that("training-style augmentations behave as defined", {
  ex <- in_example_photos()
  img <- in_read_image(ex$crop[1])
  m <- in_read_mask(ex$mask[1])
  r4 <- list(image = img, mask = m)
  for (i in 1:4) r4 <- in_augment(r4$image, r4$mask, "rotate90", value = 1)
  expect_equal(r4$image, img)
  expect_equal(r4$mask, m)
  f2 <- in_augment(in_augment(img, m, "flip", "horizontal")$image, m[, ncol(m):1], "flip", "horizontal")
  expect_equal(f2$image, img)
  for (tp in c("brightness", "contrast", "saturation")) {
    expect_equal(in_augment(img, m, tp, value = 1)$image, img, info = tp)
  }
  expect_equal(in_augment(img, m, "hue", value = 0)$image, img)
  expect_equal(in_augment(img, m, "hue", value = 1)$image, img)
  t0 <- in_traits(img, m)
  t90 <- in_traits(in_augment(img, m, "rotate90", value = 1)$image, in_augment(img, m, "rotate90", value = 1)$mask)
  expect_equal(t90$L_mean, t0$L_mean)
  expect_equal(t90$px, t0$px)
  dark <- in_augment(img, m, "brightness", value = 0.7)
  expect_lt(in_traits(dark$image, m)$L_mean, t0$L_mean)
})

test_that("augmentation check reports relative changes", {
  ex <- in_example_photos()
  chk <- in_augment_check(ex$crop[1], ex$mask[1], types = c("rotate90", "saturation"))
  expect_equal(nrow(chk), 2 * 9)
  expect_true(all(chk$rel_change[chk$type == "rotate90"] < 1e-6))
  expect_true(any(chk$rel_change[chk$type == "saturation"] > 0))
  expect_type(chk$flag, "logical")
  two <- in_augment_check(ex$crop[1:2], ex$mask[1:2], types = "flip")
  expect_equal(length(unique(two$photo_id)), 2)
})
