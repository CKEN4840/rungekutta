#' Methode de Runge-Kutta d'ordre 4
#'
#' Resout le probleme de Cauchy \eqn{y'(t) = f(t, y(t))}, \eqn{y(t_0) = y_0}
#' avec un pas fixe `h`. Fonctionne pour une equation comme pour un systeme.
#'
#' @param f Fonction `f(t, y)` qui renvoie le vecteur des derivees,
#'   de meme longueur que `y0`.
#' @param y0 Condition initiale : un nombre, ou un vecteur (eventuellement nomme)
#'   pour un systeme.
#' @param t0 Instant initial.
#' @param tf Instant final, superieur a `t0`.
#' @param h Pas de temps, strictement positif.
#'
#' @return Un data.frame de classe `solution_edo` : la colonne `t`, puis une
#'   colonne par composante de `y` (`y1`, `y2`, ... si `y0` n'est pas nomme).
#'
#' @details Chaque pas calcule quatre pentes `k1` a `k4`, puis avance de
#'   `h/6 * (k1 + 2*k2 + 2*k3 + k4)`. L'erreur globale est en O(h^4).
#'
#' @examples
#' # Equation : y' = y, y(0) = 1 (solution exacte : exp(t))
#' sol <- rk4(function(t, y) y, y0 = 1, t0 = 0, tf = 2, h = 0.1)
#' tail(sol, 1)
#' exp(2)
#'
#' # Systeme : pendule non lineaire, y = (angle, vitesse)
#' pendule <- function(t, y) c(y[2], -sin(y[1]))
#' rk4(pendule, y0 = c(theta = 1, omega = 0), t0 = 0, tf = 10, h = 0.01)[1:5, ]
#' @seealso [euler()]
#' @export
rk4 <- function(f, y0, t0, tf, h) {
  .integrer(.pas_rk4, f, y0, t0, tf, h, methode = "RK4")
}

#' Methode d'Euler
#'
#' Meme interface que [rk4()], avec la methode d'Euler (ordre 1). Utile pour
#' comparer la precision des deux methodes.
#'
#' @inheritParams rk4
#' @return Un data.frame de classe `solution_edo`, comme [rk4()].
#' @examples
#' euler(function(t, y) y, y0 = 1, t0 = 0, tf = 2, h = 0.1)
#' @seealso [rk4()]
#' @export
euler <- function(f, y0, t0, tf, h) {
  .integrer(.pas_euler, f, y0, t0, tf, h, methode = "Euler")
}
