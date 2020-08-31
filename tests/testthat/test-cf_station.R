context("cf_station")

test_that("cf_station", {
  skip_on_cran()
  # skip_on_travis()
  
  tt = cf_station()

  expect_is(tt, "cfStation")
  expect_is(tt$name, "character")
  expect_is(tt$network, "character")
  expect_is(tt$agent, "numeric")
  expect_is(tt$start, "POSIXct")
  expect_is(tt$end, "POSIXct")
  expect_is(tt$open, "logical")
  expect_is(tt$distance, "numeric")
  expect_is(tt$lat, "numeric")
  expect_is(tt$lon, "numeric")
})
