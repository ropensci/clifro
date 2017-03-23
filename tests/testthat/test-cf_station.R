context("cf_station")

test_that("cf_station", {
  tt = cf_station()
  
  expect_is(tt, "cfStation")
  expect_is(tt$name, "factor")
  expect_is(tt$network, "factor")
  expect_is(tt$agent, "numeric")
  expect_is(tt$start, "POSIXct")
  expect_is(tt$end, "POSIXct")
  expect_is(tt$open, "logical")
  expect_is(tt$distance, "numeric")
  expect_is(tt$lat, "numeric")
  expect_is(tt$lon, "numeric")
})
