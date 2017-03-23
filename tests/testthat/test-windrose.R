context("windrose")

test_that("windrose", {
  tt = windrose(speed = c(80, 2), direction = c(2, 1))

  expect_is(tt, "gg")
  expect_is(tt$data, "data.frame")
  expect_is(tt$layers, "list")
  expect_is(tt$scales, "ScalesList")
  expect_is(tt$mapping, "uneval")
  expect_is(tt$theme, "theme")
  expect_is(tt$coordinates, "CoordPolar")
  expect_is(tt$facet, "FacetNull")
  expect_is(tt$plot_env, "environment")
  expect_is(tt$labels, "list")  
})
