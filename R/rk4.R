#' Fourth-order Runge-Kutta method
#'
#' Solves the initial value problem \eqn{y'(t) = f(t, y(t))},
#' \eqn{y(t_0) = y_0} with a fixed step size `h`. Works for a single equation
#' as well as for a system of equations.
#'
#' @param f Function `f(t, y)` returning the vector of derivatives, with the
#'   same length as `y0`.
#' @param y0 Initial condition: a number, or a (possibly named) vector for a
#'   system.
#' @param t0 Initial time.
#' @param tf Final time, greater than `t0`.
#' @param h Step size, strictly positive.
#'
#' @return A data.frame of class `solution_edo`: a column `t`, then one column
#'   per component of `y` (`y1`, `y2`, ... if `y0` is not named).
#'
#' @details Each step computes four slopes `k1` to `k4`, then advances by
#'   `h/6 * (k1 + 2*k2 + 2*k3 + k4)`. The global error is O(h^4).
#'
#' @examples
#' # Equation: y' = y, y(0) = 1 (exact solution: exp(t))
#' sol <- rk4(function(t, y) y, y0 = 1, t0 = 0, tf = 2, h = 0.1)
#' tail(sol, 1)
#' exp(2)
#'
#' # System: nonlinear pendulum, y = (angle, angular velocity)
#' pendulum <- function(t, y) c(y[2], -sin(y[1]))
#' rk4(pendulum, y0 = c(theta = 1, omega = 0), t0 = 0, tf = 10, h = 0.01)[1:5, ]
#' @seealso [euler()]
#' @export
rk4 <- function(f, y0, t0, tf, h) {
  .integrer(.pas_rk4, f, y0, t0, tf, h, methode = "RK4")
}

#' Euler's method
#'
#' Same interface as [rk4()], using Euler's method (order 1). Useful to
#' compare the accuracy of the two methods.
#'
#' @inheritParams rk4
#' @return A data.frame of class `solution_edo`, as for [rk4()].
#' @examples
#' euler(function(t, y) y, y0 = 1, t0 = 0, tf = 2, h = 0.1)
#' @seealso [rk4()]
#' @export
euler <- function(f, y0, t0, tf, h) {
  .integrer(.pas_euler, f, y0, t0, tf, h, methode = "Euler")
}
