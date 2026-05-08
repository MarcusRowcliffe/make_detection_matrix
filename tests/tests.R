library(testthat)

test_that("make_edat returns expected output", {
  res <- data.frame(locationName = "c",
                    deploymentStart = ymd_hms(c("2000/01/01 00:00:00")),
                    deploymentEnd = ymd_hms(c("2000/02/01 00:00:00"))) %>%
    make_emat()
  expect_true(inherits(res, "list"))
  expect_all_true(c("effort", "cuts") %in% names(res))
})

test_that("make_edat fails if required data missing", {
  deployments <- data.frame(deploymentStart = ymd_hms(c("2000/01/01 00:00:00")),
                            deploymentEnd = ymd_hms(c("2000/02/01 00:00:00")))
  expect_error(make_emat(deployments))
})

