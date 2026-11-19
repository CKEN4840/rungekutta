test_that("the SIR model conserves the total population", {
  f   <- modele_sir(0.3, 0.1, 1000)
  sol <- rk4(f, c(S = 999, I = 1, R = 0), 0, 160, 0.5)
  expect_equal(unname(rowSums(sol[, c("S", "I", "R")])), rep(1000, nrow(sol)))
  expect_equal(sol$t[which.max(sol$I)], 38.5)   # peak at day 38.5
})

test_that("invalid parameters are rejected", {
  expect_error(modele_sir(beta = -1, gamma = 0.1, N = 1000))
})
