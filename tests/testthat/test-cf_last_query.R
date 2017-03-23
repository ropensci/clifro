context("cf_last_query")

test_that("cf_last_query", {
  tt = cf_query(cf_user("public"), cf_datatype(5, 2, 1), cf_station(), 
                "2012-01-01 00", "2012-01-02 00")
  tt2 = cf_last_query()
  
  expect_is(tt2, "cfSunshine")
  expect_is(tt2$Station, "factor")
  expect_is(tt2$`Date(local)`, "POSIXct")
  expect_is(tt2$`Amount(MJ/m2)`, "numeric")
  expect_is(tt2$`Period(Hrs)`, "integer")
  expect_is(tt2$Type, "factor")
  expect_is(tt2$Freq, "factor")
})
