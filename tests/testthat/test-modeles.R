test_that("le modele SIR conserve la population totale", {
  f   <- modele_sir(0.3, 0.1, 1000)
  sol <- rk4(f, c(S = 999, I = 1, R = 0), 0, 160, 0.5)
  expect_equal(unname(rowSums(sol[, c("S", "I", "R")])), rep(1000, nrow(sol)))
  expect_equal(sol$t[which.max(sol$I)], 38.5)   # pic au jour 38,5
})

test_that("Lotka-Volterra est immobile a l'equilibre", {
  f <- modele_lotka_volterra(1, 0.1, 0.075, 1.5)
  expect_equal(f(0, c(20, 10)), c(0, 0))        # x* = gamma/delta, y* = alpha/beta
})

test_that("RK4 conserve presque l'invariant de Lotka-Volterra", {
  f   <- modele_lotka_volterra(1, 0.1, 0.075, 1.5)
  sol <- rk4(f, c(proies = 10, predateurs = 5), 0, 30, 0.01)
  H   <- invariant_lotka_volterra(sol$proies, sol$predateurs, 1, 0.1, 0.075, 1.5)
  expect_lt(abs(H[length(H)] - H[1]), 1e-6)
})

test_that("les parametres invalides sont refuses", {
  expect_error(modele_sir(beta = -1, gamma = 0.1, N = 1000))
})

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
