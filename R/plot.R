#' Tracer une solution
#'
#' Trace chaque composante de la solution en fonction du temps.
#'
#' @param x Un objet `solution_edo`, renvoye par [rk4()] ou [euler()].
#' @param y Inutilise (present pour rester coherent avec [plot()]).
#' @param ... Inutilise.
#' @return Un objet `ggplot`, que l'on peut modifier avec `+`.
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
    ggplot2::labs(title = paste("Solution par", attr(x, "methode")),
                  x = "t", y = NULL, colour = NULL) +
    .theme_rk()
}

#' Comparer plusieurs solutions
#'
#' Superpose des solutions (par exemple Euler, RK4 et la solution exacte).
#' Chaque argument doit etre nomme ; ce nom apparait dans la legende. On peut
#' aussi passer un data.frame ordinaire ayant une colonne `t`.
#'
#' @param ... Solutions nommees, par exemple `Euler = e, RK4 = r`.
#' @param variable Noms des composantes a tracer (toutes par defaut).
#' @return Un objet `ggplot`.
#' @examples
#' f <- function(t, y) y
#' e <- euler(f, 1, 0, 2, 0.2)
#' r <- rk4(f, 1, 0, 2, 0.2)
#' exacte <- data.frame(t = e$t, y1 = exp(e$t))
#' comparer_solutions(Exacte = exacte, Euler = e, RK4 = r)
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

#' Portrait de phase
#'
#' Trace une composante en fonction d'une autre (par exemple predateurs en
#' fonction des proies). Utile pour voir si une methode reste sur une orbite fermee.
#'
#' @param ... Solutions nommees, comme pour [comparer_solutions()].
#' @param x,y Noms (en texte) des colonnes a mettre en abscisse et en ordonnee.
#' @return Un objet `ggplot`.
#' @examples
#' f <- modele_lotka_volterra(1, 0.1, 0.075, 1.5)
#' y0 <- c(proies = 10, predateurs = 5)
#' portrait_de_phase(Euler = euler(f, y0, 0, 30, 0.01),
#'                   RK4   = rk4(f, y0, 0, 30, 0.01),
#'                   x = "proies", y = "predateurs")
#' @export
portrait_de_phase <- function(..., x, y) {
  sols <- .solutions_nommees(...)
  d <- do.call(rbind, lapply(names(sols), function(nm) {
    data.frame(source = nm, abscisse = sols[[nm]][[x]], ordonnee = sols[[nm]][[y]])
  }))
  d$source <- factor(d$source, levels = names(sols))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$abscisse, y = .data$ordonnee,
                                  colour = .data$source)) +
    ggplot2::geom_path(linewidth = 1) +
    ggplot2::scale_colour_manual(values = rep_len(.couleurs, nlevels(d$source))) +
    ggplot2::labs(x = x, y = y, colour = NULL, title = "Portrait de phase") +
    .theme_rk()
}
