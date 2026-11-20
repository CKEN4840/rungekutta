#' Launch the SIR epidemic simulator
#'
#' Opens the Shiny decision-support app bundled with the package.
#'
#' @param ... Arguments passed to [shiny::runApp()].
#' @return Called for its side effect: it starts the app.
#' @export
lancer_simulateur <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required: install.packages(\"shiny\")",
         call. = FALSE)
  }
  chemin <- system.file("app", package = "rungekutta")
  if (chemin == "") {
    stop("App directory not found. Try reinstalling 'rungekutta'.",
         call. = FALSE)
  }
  shiny::runApp(chemin, ...)
}
