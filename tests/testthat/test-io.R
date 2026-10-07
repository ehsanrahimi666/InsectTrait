test_that("example photos are shipped with attribution", {
  ex <- in_example_photos()
  expect_equal(nrow(ex), 7)
  expect_true(all(file.exists(c(ex$photo, ex$crop, ex$mask))))
  expect_true(all(grepl("publicdomain/zero|licenses/by/", ex$license)))
  expect_setequal(unique(ex$order), c("Coleoptera", "Diptera", "Hymenoptera", "Lepidoptera"))
  expect_false(any(is.na(ex$rights_holder) | ex$rights_holder == ""))
})

test_that("images are read as 0-255 RGB arrays", {
  ex <- in_example_photos()
  img <- in_read_image(ex$photo[1])
  expect_equal(length(dim(img)), 3L)
  expect_equal(dim(img)[3], 3L)
  expect_equal(max(dim(img)[1:2]), 800)
  expect_true(all(img >= 0 & img <= 255))
  expect_equal(img, round(img))
  small <- in_read_image(ex$photo[1], max_side = 200)
  expect_equal(max(dim(small)[1:2]), 200)
  expect_equal(round(mean(small)), round(mean(img)))
})

test_that("grey, alpha and 0/1 PNG files are handled", {
  f <- tempfile(fileext = ".png")
  g <- matrix(seq(0, 1, length.out = 20), 4, 5)
  png::writePNG(g, f)
  img <- in_read_image(f)
  expect_equal(dim(img), c(4, 5, 3))
  expect_equal(img[, , 1], img[, , 3])
  png::writePNG(array(0.5, c(3, 3, 4)), f)
  expect_equal(dim(in_read_image(f)), c(3, 3, 3))
  m <- matrix(c(0, 1, 1, 0), 2) / 255
  png::writePNG(m, f)
  expect_equal(in_read_mask(f), m > 0)
  expect_error(in_read_image(tempfile()), "not found")
  bad <- tempfile(fileext = ".txt")
  writeLines("x", bad)
  expect_error(in_read_image(bad), "JPEG and PNG")
})

test_that("masks round-trip through PNG", {
  m <- matrix(FALSE, 30, 40)
  m[5:20, 10:30] <- TRUE
  f <- tempfile(fileext = ".png")
  in_write_mask(m, f)
  expect_identical(in_read_mask(f), m)
  expect_error(in_write_mask(1:3, f), "matrix")
})

test_that("area resize preserves the mean", {
  set.seed(2)
  M <- matrix(runif(37 * 53), 37)
  r <- InsectTrait:::.in_area_resize(M, 11, 16)
  expect_equal(dim(r), c(11, 16))
  expect_equal(mean(r), mean(M), tolerance = 1e-12)
})
