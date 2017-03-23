context("cf_curl_opts")

test_that("cf_curl_opts", {
  tt <- cf_curl_opts()
  
  expect_is(tt, "list")
  expect_is(tt$followlocation,"logical")
  expect_is(tt$useragent, "character")
  expect_is(tt$timeout, "numeric")
})
