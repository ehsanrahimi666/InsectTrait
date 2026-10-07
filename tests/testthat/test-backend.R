test_that("back end processes an example photo", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("INSECTTRAIT_TEST_BACKEND"), "true"),
              "set INSECTTRAIT_TEST_BACKEND=true to test the Python back end")
  skip_if_not(in_backend_available())
  ex <- in_example_photos()
  out <- file.path(tempdir(), "in_backend_test")
  res <- in_process(ex$photo[1], out_dir = out)
  expect_equal(nrow(res), 1)
  expect_true(all(c("det_score", "stab", "p_usable_any", "L_mean", "glcm_contrast") %in% names(res)))
  r <- in_traits(file.path(out, "crops", paste0(res$photo_id, ".jpg")),
                 file.path(out, "masks", paste0(res$photo_id, ".png")))
  expect_equal(r$L_mean, res$L_mean, tolerance = 1e-3)
})

test_that("non-insect prompts warn", {
  expect_silent(InsectTrait:::.in_check_prompt("insect"))
  expect_silent(InsectTrait:::.in_check_prompt("bee. hoverfly. butterfly"))
  expect_warning(InsectTrait:::.in_check_prompt("bird"), "insects only")
  expect_warning(InsectTrait:::.in_check_prompt(""), "insects only")
})

test_that("photo inputs are turned into a table", {
  ex <- in_example_photos()
  it <- InsectTrait:::.in_items(ex$photo[1:2])
  expect_equal(it$photo_id, ex$photo_id[1:2])
  it2 <- InsectTrait:::.in_items(dirname(ex$photo[1]))
  expect_equal(nrow(it2), 7)
  df <- data.frame(photo_id = "a", url = "https://example.org/x.jpg", order = "Diptera")
  it3 <- InsectTrait:::.in_items(df)
  expect_equal(it3$path, "https://example.org/x.jpg")
  expect_error(InsectTrait:::.in_items("no_such_file.jpg"), "not found")
  expect_error(InsectTrait:::.in_items(data.frame(photo_id = c("a", "a"), url = c("u", "v"))),
               "unique")
})
