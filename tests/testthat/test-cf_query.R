context("cf_query")

test_that("cf_query", {
  tt = cf_query(cf_user("public"), cf_datatype(5, 2, 1), cf_station(), 
                "2012-01-01 00", "2012-01-02 00")

  expect_is(tt, "cfSunshine")
  expect_is(tt$Station, "factor")
  expect_is(tt$`Date(local)`, "POSIXct")
  expect_is(tt$`Amount(MJ/m2)`, "numeric")
  expect_is(tt$`Period(Hrs)`, "integer")
  expect_is(tt$Type, "factor")
  expect_is(tt$Freq, "factor")
})
