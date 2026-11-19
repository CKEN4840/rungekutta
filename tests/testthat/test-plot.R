test_that("plotting functions return ggplot objects", {
  f <- function(t, y) y
  e <- euler(f, 1, 0, 2, 0.2)
  r <- rk4(f, 1, 0, 2, 0.2)
  expect_s3_class(plot(r), "ggplot")
  expect_s3_class(comparer_solutions(Euler = e, RK4 = r), "ggplot")
})

test_that("comparer_solutions requires names", {
  r <- rk4(function(t, y) y, 1, 0, 2, 0.2)
  expect_error(comparer_solutions(r))
})
