message("\n---- Test get_site_boundaries() ----")

library(testthat)

test_that("Expect error if internet connection is down", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  testthat::expect_error(
    httptest::without_internet(
      result <- ReLTER:::get_site_boundaries(
        deimsid = TESTURLSite
      )
    ),
    "GET"
  )
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

skip_if_offline(host = "deims.org")

test_that("Output of site boundaries function constructs 'sf' and 'tibble' as
          expected", {
  result <- ReLTER:::get_site_boundaries(
    deimsid = TESTURLSite
  )
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) == 3)
  expect_true(all(names(result) == c(
    "title", "uri", "boundaries"
  )))
  expect_type(result$title, "character")
  expect_type(result$uri, "character")
  if (is.na(result$boundaries)) {
    expect_type(result$boundaries, "logical")
  } else {
    expect_type(result$boundaries, "list")
  }
})

test_that("Wrong input (but URL) constructs a NULL object", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER:::get_site_boundaries(
    deimsid = "https://deims.org/ljhnhbkihubib"
  )
  expect_type(result, "NULL")
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})

test_that("Wrong input (not URL) constructs an empty tibble", {
  Sys.setenv("LOCAL_DEIMS" = FALSE) # set online mode
  result <- ReLTER:::get_site_boundaries(deimsid = "ljhnhbkihubib")
  expect_type(result, "NULL")
  Sys.setenv("LOCAL_DEIMS" = test_mode) # restore test mode
})
