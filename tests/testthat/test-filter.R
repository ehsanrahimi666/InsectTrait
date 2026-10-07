test_that("decision rules reproduce the validation", {
  n <- vapply(c("qc", "default", "strict", "top_view"),
              function(r) sum(in_filter(in_validation, r)$keep), numeric(1))
  expect_equal(unname(n), c(773, 552, 438, 314))
  b <- in_validation[in_validation$role == "benchmark", ]
  kept <- vapply(c("default", "strict", "top_view"), function(r) mean(in_filter(b, r)$keep),
                 numeric(1))
  expect_equal(unname(round(kept, 3)), c(0.706, 0.56, 0.404))
  k <- in_filter(b, "default")$keep
  expect_equal(round(mean(b$outline_rating[k] == "wrong"), 3), 0.009)
})

test_that("reasons follow the order of checks", {
  base <- data.frame(mask_ok = TRUE, stab = 0.95, n_other_insects = 0, p_usable_any = 0.9,
                     p_top_view = 0.8, p_upperside_lep = 0.2, order = "Coleoptera")
  x <- base[rep(1, 6), ]
  x$mask_ok[1] <- FALSE
  x$stab[2] <- 0.5
  x$n_other_insects[3] <- 2
  x$p_usable_any[4] <- 0.2
  x$order[5] <- "Lepidoptera"
  x$p_top_view[6] <- 0.1
  d <- in_filter(x, "default")
  expect_equal(d$reason, c("outline failed QC", "top view", "several insects",
                           "unclear photo (hidden, blurry or cut off)", "underside view",
                           "side view"))
  expect_equal(d$keep, c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE))
  s <- in_filter(x, "strict")
  expect_equal(s$reason[2], "unstable outline")
  tv <- in_filter(x, "top_view")
  expect_equal(tv$keep, c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(tv$reason[5:6], c("underside view", "side view"))
  expect_true(all(in_filter(x, "none")$keep))
  expect_error(in_filter(x[, -1], "default"), "mask_ok")
  expect_error(in_filter(1:3), "data frame")
})

test_that("missing values fail the checks and text logicals are read", {
  x <- data.frame(mask_ok = c("True", "False", NA), stab = NA, n_other_insects = 0,
                  p_usable_any = c(0.9, 0.9, 0.9), p_top_view = 0.9, p_upperside_lep = 0.9)
  d <- in_filter(x, "default")
  expect_equal(d$keep, c(TRUE, FALSE, FALSE))
  expect_false(in_filter(x, "strict")$keep[1])
})

test_that("results objects print, summarise and plot", {
  res <- in_example_results()
  expect_s3_class(res, "in_results")
  expect_equal(nrow(res), 7)
  expect_equal(res$reason[res$photo_id == "6273727788_01"], "underside view")
  expect_equal(res$reason[res$photo_id == "6452014113_01"], "several insects")
  expect_equal(sum(res$keep), 5)
  expect_equal(sum(in_example_results("strict")$keep), 5)
  expect_output(print(res), "in_results")
  s <- summary(res)
  expect_equal(sum(s$n), 7)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(plot(res))
})

test_that("subsetting results keeps the rule", {
  r <- in_example_results("strict")
  s <- r[r$keep, c("photo_id", "keep", "reason")]
  expect_s3_class(s, "in_results")
  expect_identical(attr(s, "rule"), "strict")
  expect_output(print(s), "rule: strict")
  expect_false(inherits(r[, c("photo_id", "L_mean")], "in_results"))
  expect_type(r[, "L_mean"], "double")
})
