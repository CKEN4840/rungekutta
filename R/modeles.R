#' Modele epidemique SIR
#'
#' Renvoie la fonction `f(t, y)` du modele SIR, a donner a [rk4()] ou [euler()].
#' L'etat est `y = c(S, I, R)` (sains, infectes, gueris) avec
#' S' = -beta*S*I/N, I' = beta*S*I/N - gamma*I, R' = gamma*I.
#'
#' @param beta Taux de transmission (strictement positif).
#' @param gamma Taux de guerison (strictement positif).
#' @param N Population totale (strictement positive).
#' @return Une fonction `f(t, y)` qui renvoie `c(S', I', R')`.
#' @examples
#' f   <- modele_sir(beta = 0.3, gamma = 0.1, N = 1000)
#' sol <- rk4(f, y0 = c(S = 999, I = 1, R = 0), t0 = 0, tf = 160, h = 0.5)
#' sol[which.max(sol$I), ]   # pic de l'epidemie
#' @export
modele_sir <- function(beta, gamma, N) {
  .verifier_positifs(beta = beta, gamma = gamma, N = N)
  function(t, y) {
    S <- y[1]; I <- y[2]
    c(-beta * S * I / N,
      beta * S * I / N - gamma * I,
      gamma * I)
  }
}

#' Modele proies-predateurs de Lotka-Volterra
#'
#' Renvoie la fonction `f(t, y)` du modele, avec `y = c(proies, predateurs)` :
#' x' = alpha*x - beta*x*y et y' = delta*x*y - gamma*y.
#'
#' @param alpha Taux de croissance des proies.
#' @param beta Taux de predation.
#' @param delta Taux de croissance des predateurs grace aux proies.
#' @param gamma Taux de mortalite des predateurs.
#' @return Une fonction `f(t, y)` qui renvoie `c(x', y')`.
#' @examples
#' f   <- modele_lotka_volterra(alpha = 1, beta = 0.1, delta = 0.075, gamma = 1.5)
#' sol <- rk4(f, y0 = c(proies = 10, predateurs = 5), t0 = 0, tf = 30, h = 0.01)
#' head(sol)
#' @export
modele_lotka_volterra <- function(alpha, beta, delta, gamma) {
  .verifier_positifs(alpha = alpha, beta = beta, delta = delta, gamma = gamma)
  function(t, y) {
    x <- y[1]; p <- y[2]
    c(alpha * x - beta * x * p,
      delta * x * p - gamma * p)
  }
}

#' Quantite conservee du modele de Lotka-Volterra
#'
#' La quantite `delta*x - gamma*log(x) + beta*y - alpha*log(y)` est constante
#' le long d'une solution exacte. Sa derive mesure l'erreur numerique d'une methode.
#'
#' @param x,y Populations de proies et de predateurs (vecteurs).
#' @inheritParams modele_lotka_volterra
#' @return Un vecteur de meme longueur que `x`.
#' @export
invariant_lotka_volterra <- function(x, y, alpha, beta, delta, gamma) {
  delta * x - gamma * log(x) + beta * y - alpha * log(y)
}
