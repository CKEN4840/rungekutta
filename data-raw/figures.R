# Genere les images du diaporama dans le dossier figures/
devtools::load_all()
library(ggplot2)
dir.create("figures", showWarnings = FALSE)

enregistrer <- function(p, nom) {
  ggsave(file.path("figures", nom), p, width = 9, height = 5.5, dpi = 300, bg = "white")
}

# 1. y' = y : exacte, Euler, RK4
f <- function(t, y) y
e <- euler(f, 1, 0, 2, 0.2)
r <- rk4(f,   1, 0, 2, 0.2)
exacte <- data.frame(t = e$t, y1 = exp(e$t))
enregistrer(
  comparer_solutions(Exacte = exacte, Euler = e, RK4 = r) +
    labs(title = "y' = y : solution exacte, Euler et RK4 (h = 0,2)"),
  "comparaison_rk4.png"
)

# 2. Convergence
pas <- c(0.5, 0.25, 0.1, 0.05, 0.025)
erreur <- function(methode, h) abs(tail(methode(f, 1, 0, 2, h)$y1, 1) - exp(2))
err <- data.frame(
  h       = rep(pas, 2),
  methode = rep(c("Euler", "RK4"), each = length(pas)),
  erreur  = c(sapply(pas, function(h) erreur(euler, h)),
              sapply(pas, function(h) erreur(rk4,   h)))
)
enregistrer(
  ggplot(err, aes(h, erreur, colour = methode)) +
    geom_line(linewidth = 1) + geom_point(size = 3) +
    scale_x_log10() + scale_y_log10() +
    scale_colour_manual(values = c("#B04E08", "#1F7A8C")) +
    labs(title = "Convergence (echelle log-log)", colour = NULL) +
    theme_minimal(base_size = 14) + theme(legend.position = "top"),
  "convergence.png"
)

# 3. SIR
sir <- rk4(modele_sir(0.3, 0.1, 1000), c(S = 999, I = 1, R = 0), 0, 160, 0.5)
enregistrer(plot(sir) + labs(title = "Modele SIR resolu par RK4", x = "Temps (jours)"),
            "sir.png")

# 4. Lotka-Volterra
f_lv <- modele_lotka_volterra(1, 0.1, 0.075, 1.5)
y0   <- c(proies = 10, predateurs = 5)
lv_e <- euler(f_lv, y0, 0, 30, 0.01)
lv_r <- rk4(f_lv,   y0, 0, 30, 0.01)
enregistrer(plot(lv_r) + labs(title = "Lotka-Volterra (RK4)"), "lotka_volterra.png")
enregistrer(portrait_de_phase(Euler = lv_e, RK4 = lv_r, x = "proies", y = "predateurs"),
            "portrait_de_phase.png")
