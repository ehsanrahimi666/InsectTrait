## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.
* Examples that need the optional Python back end or internet access are
  wrapped in `\dontrun{}`; all other examples run in a few seconds.
* Tests of the Python back end run only when the environment variable
  `INSECTTRAIT_TEST_BACKEND` is `"true"`; tests that query GBIF or iNaturalist
  are skipped on CRAN, on continuous integration and when offline.
