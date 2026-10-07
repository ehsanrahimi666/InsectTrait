fixture <- function(f) jsonlite::read_json(test_path("fixtures", f), simplifyVector = FALSE)

test_that("occurrence records are parsed into one row per photo", {
  js <- fixture("gbif_occurrences.json")
  p <- InsectTrait:::.in_parse_occurrences(js)
  expect_equal(p$photo_id, c("5938036426_01", "5938074161_01"))
  expect_equal(p$license, c("CC0_1_0", "CC_BY_4_0"))
  expect_true(all(grepl("^https://", p$url)))
  expect_equal(unique(p$species), "Harmonia axyridis")
  p3 <- InsectTrait:::.in_parse_occurrences(js, max_per_occurrence = 3)
  expect_equal(nrow(p3), 4)
  expect_equal(sum(p3$occurrenceKey == "5938074161"), 3)
  pnc <- InsectTrait:::.in_parse_occurrences(js, license = c("CC0_1_0", "CC_BY_4_0", "CC_BY_NC_4_0"))
  expect_equal(nrow(pnc), 4)
  empty <- InsectTrait:::.in_parse_occurrences(list(results = list()))
  expect_equal(nrow(empty), 0)
  expect_named(empty, names(p))
})

test_that("only insect taxa are accepted", {
  expect_equal(InsectTrait:::.in_check_insect(fixture("gbif_match_insect.json")), 4989904)
  expect_error(InsectTrait:::.in_check_insect(fixture("gbif_match_bird.json")), "insects only")
  expect_error(InsectTrait:::.in_check_insect(list(matchType = "NONE")), "No GBIF")
})

test_that("licences are mapped and attribution lines are built", {
  expect_equal(InsectTrait:::.in_license_code(c("http://creativecommons.org/publicdomain/zero/1.0/",
                                                "http://creativecommons.org/licenses/by/4.0/",
                                                "http://creativecommons.org/licenses/by-nc/4.0/",
                                                "CC-BY", NA)),
               c("CC0_1_0", "CC_BY_4_0", "CC_BY_NC_4_0", "CC_BY_4_0", "other"))
  a <- in_attribution(in_example_photos())
  expect_length(a, 7)
  expect_true(all(grepl("Photo: .+ \\((CC0 1.0|CC BY 4.0)\\)", a)))
})

test_that("live GBIF search returns insect photos", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline("api.gbif.org")
  p <- in_gbif_photos("Harmonia axyridis", n = 3)
  expect_lte(nrow(p), 3)
  expect_true(all(p$license %in% c("CC0_1_0", "CC_BY_4_0")))
  expect_error(in_gbif_photos("Parus major", n = 1), "insects only")
})
