#' Plot a solution
#'
#' Plots each component of the solution against time.
#'
#' @param x An object of class `solution_edo`, returned by [rk4()] or [euler()].
#' @param y Unused (kept for consistency with [plot()]).
#' @param ... Unused.
#' @return A `ggplot` object, which can be modified with `+`.
#' @examples
#' sol <- rk4(function(t, y) c(y[2], -y[1]), c(x = 1, v = 0), 0, 10, 0.05)
#' plot(sol)
#' @export
plot.solution_edo <- function(x, y, ...) {
  d <- .vers_long(x)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$t, y = .data$valeur,
                                  colour = .data$variable)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_colour_manual(values = rep_len(.couleurs, nlevels(d$variable))) +
    ggplot2::labs(title = paste("Solution by", attr(x, "methode")),
                  x = "t", y = NULL, colour = NULL) +
    .theme_rk()
}

#' Compare several solutions
#'
#' Overlays several solutions (for example Euler, RK4 and the exact solution).
#' Every argument must be named; the name appears in the legend. An ordinary
#' data.frame with a column `t` can also be supplied.
#'
#' @param ... Named solutions, for example `Euler = e, RK4 = r`.
#' @param variable Names of the components to plot (all by default).
#' @return A `ggplot` object.
#' @examples
#' f <- function(t, y) y
#' e <- euler(f, 1, 0, 2, 0.2)
#' r <- rk4(f, 1, 0, 2, 0.2)
#' exact <- data.frame(t = e$t, y1 = exp(e$t))
#' comparer_solutions(Exact = exact, Euler = e, RK4 = r)
#' @export
comparer_solutions <- function(..., variable = NULL) {
  sols <- .solutions_nommees(...)
  d <- do.call(rbind, lapply(names(sols), function(nm) {
    long <- .vers_long(sols[[nm]])
    long$source <- nm
    long
  }))
  d$source <- factor(d$source, levels = names(sols))
  if (!is.null(variable)) d <- d[d$variable %in% variable, ]

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$t, y = .data$valeur,
                                       colour = .data$source)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_colour_manual(values = rep_len(.couleurs, nlevels(d$source))) +
    ggplot2::labs(x = "t", y = NULL, colour = NULL) +
    .theme_rk()
  if (nlevels(droplevels(d$variable)) > 1) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$variable), scales = "free_y")
  }
  p
}
