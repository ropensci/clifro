context("cf_last_query")

test_that("cf_last_query", {
  tt <- cf_last_query()
  
  expect_is(tt, "cfSunshine")
  expect_is(tt$Station, "factor")
  expect_is(tt$`Date(local)`, "POSIXct")
  expect_is(tt$`Amount(MJ/m2)`, "numeric")
  expect_is(tt$`Period(Hrs)`, "integer")
  expect_is(tt$Type, "factor")
  expect_is(tt$Freq, "factor")
})
