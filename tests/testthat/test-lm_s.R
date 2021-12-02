test_that("multiplication works", {
  y=1:2
  x=1:2
  expect_equal(lm_s(y~x), -1)
  y = 1:3
  x = 1:3
  expect_warning(lm_s(y~x))
  y=1:2
  x=1:2
  z=1:2
  expect_equal(lm_s(y~x+z), -1)
  y=1:1000
  x=1:1000
  z=1:1000
  expect_equal(lm_s(y~x+z), -1)
  y=1:1000
  x=rnorm(1000)
  z=rnorm(1000)
  expect_length(lm_s(y~x+z), 3)
  y=1:1000
  x=1:1000
  z=rnorm(1000)
  expect_length(lm_s(y~z), 2)
})
