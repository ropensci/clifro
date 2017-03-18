context("cf_curl_opts")

test_that("cf_curl_opts", {
  tt <- cf_curl_opts()
  
  expect_is(tt, "list")
})
