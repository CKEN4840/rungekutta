test_that("les fonctions de trace renvoient des objets ggplot", {
  f <- function(t, y) y
  e <- euler(f, 1, 0, 2, 0.2)
  r <- rk4(f, 1, 0, 2, 0.2)
  expect_s3_class(plot(r), "ggplot")
  expect_s3_class(comparer_solutions(Euler = e, RK4 = r), "ggplot")

  g   <- modele_lotka_volterra(1, 0.1, 0.075, 1.5)
  y0  <- c(proies = 10, predateurs = 5)
  sol <- rk4(g, y0, 0, 5, 0.1)
  expect_s3_class(portrait_de_phase(RK4 = sol, x = "proies", y = "predateurs"), "ggplot")
})

test_that("comparer_solutions exige des noms", {
  r <- rk4(function(t, y) y, 1, 0, 2, 0.2)
  expect_error(comparer_solutions(r))
})

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
