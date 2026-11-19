#' SIR epidemic model
#'
#' Returns the function `f(t, y)` of the SIR model, to be passed to [rk4()] or
#' [euler()]. The state is `y = c(S, I, R)` (susceptible, infected, recovered)
#' with S' = -beta*S*I/N, I' = beta*S*I/N - gamma*I and R' = gamma*I.
#'
#' @param beta Transmission rate (strictly positive).
#' @param gamma Recovery rate (strictly positive).
#' @param N Total population (strictly positive).
#' @return A function `f(t, y)` returning `c(S', I', R')`.
#' @examples
#' f   <- modele_sir(beta = 0.3, gamma = 0.1, N = 1000)
#' sol <- rk4(f, y0 = c(S = 999, I = 1, R = 0), t0 = 0, tf = 160, h = 0.5)
#' sol[which.max(sol$I), ]   # epidemic peak
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
