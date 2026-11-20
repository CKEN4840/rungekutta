test_that("the simulator app is bundled with the package", {
  expect_true(file.exists(system.file("app", "app.R", package = "rungekutta")))
})

test_that("lancer_simulateur is exported", {
  expect_true("lancer_simulateur" %in% getNamespaceExports("rungekutta"))
})
