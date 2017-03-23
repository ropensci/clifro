context("cf_user")

test_that("cf_user", {
  tt = cf_user(username = "public")

  expect_is(tt, "cfUser") 
})
