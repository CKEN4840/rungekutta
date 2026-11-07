test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("rk4 est precis sur y' = y", {
  sol <- rk4(function(t, y) y, 1, 0, 2, 0.05)
  expect_lt(abs(tail(sol$y1, 1) - exp(2)), 1e-5)
})

test_that("rk4 est bien plus precis qu'euler", {
  f <- function(t, y) y
  err_rk4   <- abs(tail(rk4(f,   1, 0, 2, 0.05)$y1, 1) - exp(2))
  err_euler <- abs(tail(euler(f, 1, 0, 2, 0.05)$y1, 1) - exp(2))
  expect_lt(err_rk4, err_euler / 1000)
})

test_that("un systeme donne une colonne par composante", {
  sol <- rk4(function(t, y) c(y[2], -y[1]), c(1, 0), 0, 1, 0.1)
  expect_equal(ncol(sol), 3)   # t, y1, y2
})

test_that("les arguments invalides sont refuses", {
  expect_error(rk4(function(t, y) y, 1, 0, 2, -0.1))
  expect_error(rk4(function(t, y) y, 1, 2, 0, 0.1))
})
