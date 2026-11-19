# Generates the images used in the README and slides, in the figures/ folder
devtools::load_all()
library(ggplot2)
dir.create("figures", showWarnings = FALSE)

save_plot <- function(p, name) {
  ggsave(file.path("figures", name), p, width = 9, height = 5.5, dpi = 300, bg = "white")
}

# 1. y' = y : exact solution, Euler, RK4
f <- function(t, y) y
e <- euler(f, 1, 0, 2, 0.2)
r <- rk4(f,   1, 0, 2, 0.2)
exact <- data.frame(t = e$t, y1 = exp(e$t))
save_plot(
  comparer_solutions(Exact = exact, Euler = e, RK4 = r) +
    labs(title = "y' = y: exact solution, Euler and RK4 (h = 0.2)"),
  "comparaison_rk4.png"
)

# 2. Convergence
steps <- c(0.5, 0.25, 0.1, 0.05, 0.025)
error <- function(method, h) abs(tail(method(f, 1, 0, 2, h)$y1, 1) - exp(2))
err <- data.frame(
  h      = rep(steps, 2),
  method = rep(c("Euler", "RK4"), each = length(steps)),
  error  = c(sapply(steps, function(h) error(euler, h)),
             sapply(steps, function(h) error(rk4,   h)))
)
save_plot(
  ggplot(err, aes(h, error, colour = method)) +
    geom_line(linewidth = 1) + geom_point(size = 3) +
    scale_x_log10() + scale_y_log10() +
    scale_colour_manual(values = c("#B04E08", "#1F7A8C")) +
    labs(title = "Convergence (log-log scale)", colour = NULL) +
    theme_minimal(base_size = 14) + theme(legend.position = "top"),
  "convergence.png"
)

# 3. SIR
sir <- rk4(modele_sir(0.3, 0.1, 1000), c(S = 999, I = 1, R = 0), 0, 160, 0.5)
save_plot(plot(sir) + labs(title = "SIR model solved with RK4", x = "Time (days)"),
          "sir.png")
