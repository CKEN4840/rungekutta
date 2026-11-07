# Fonctions internes (non exportées) -------------------------------------

# Un pas d'Euler
.pas_euler <- function(f, t, y, h) {
  y + h * f(t, y)
}

# Un pas de Runge-Kutta d'ordre 4
.pas_rk4 <- function(f, t, y, h) {
  k1 <- f(t,       y)
  k2 <- f(t + h/2, y + h/2 * k1)
  k3 <- f(t + h/2, y + h/2 * k2)
  k4 <- f(t + h,   y + h   * k3)
  y + h/6 * (k1 + 2*k2 + 2*k3 + k4)
}

# Boucle commune : applique `pas` de t0 a tf avec un pas h
.integrer <- function(pas, f, y0, t0, tf, h, methode) {
  stopifnot(
    "f doit etre une fonction f(t, y)"    = is.function(f),
    "y0 doit etre numerique"              = is.numeric(y0) && length(y0) >= 1,
    "t0, tf et h doivent etre des nombres" =
      is.numeric(t0) && length(t0) == 1 &&
      is.numeric(tf) && length(tf) == 1 &&
      is.numeric(h)  && length(h)  == 1,
    "h doit etre strictement positif"     = h > 0,
    "tf doit etre superieur a t0"         = tf > t0
  )

  n <- round((tf - t0) / h)
  if (abs(t0 + n * h - tf) > 1e-8 * max(1, abs(tf))) {
    warning("(tf - t0) n'est pas un multiple de h : le calcul s'arrete en t = ",
            t0 + n * h, call. = FALSE)
  }

  # f doit renvoyer un vecteur de meme longueur que y0
  if (length(f(t0, y0)) != length(y0)) {
    stop("f(t, y) doit renvoyer un vecteur de meme longueur que y0")
  }

  t <- t0 + (0:n) * h
  Y <- matrix(NA_real_, nrow = n + 1, ncol = length(y0))
  Y[1, ] <- y0
  for (i in seq_len(n)) {
    Y[i + 1, ] <- pas(f, t[i], Y[i, ], h)
  }

  noms <- if (!is.null(names(y0))) names(y0) else paste0("y", seq_along(y0))
  colnames(Y) <- noms

  structure(
    data.frame(t = t, Y),
    methode = methode,
    class   = c("solution_edo", "data.frame")
  )
}

# Verifie que tous les parametres sont des nombres strictement positifs
.verifier_positifs <- function(...) {
  p  <- list(...)
  ok <- vapply(p, function(v) is.numeric(v) && length(v) == 1 && v > 0, logical(1))
  if (!all(ok)) {
    stop("Nombre strictement positif attendu pour : ",
         paste(names(p)[!ok], collapse = ", "), call. = FALSE)
  }
}

# Palette utilisee par les graphiques (bleu nuit, ambre, bleu-vert, gris...)
.couleurs <- c("#14213D", "#B04E08", "#1F7A8C", "#6A7179", "#C0392B", "#7D5BA6")

# Passe une solution au format "long" : une ligne par (t, variable)
.vers_long <- function(sol) {
  d    <- as.data.frame(sol)
  vars <- setdiff(names(d), "t")
  out  <- do.call(rbind, lapply(vars, function(v) {
    data.frame(t = d$t, variable = v, valeur = d[[v]])
  }))
  out$variable <- factor(out$variable, levels = vars)
  out
}

# Verifie que les solutions passees a `...` sont nommees
.solutions_nommees <- function(...) {
  sols  <- list(...)
  noms  <- names(sols)
  if (length(sols) == 0 || is.null(noms) || any(noms == "")) {
    stop("Donner un nom a chaque solution, par exemple ",
         "comparer_solutions(Euler = e, RK4 = r)", call. = FALSE)
  }
  sols
}

# Theme commun a tous les graphiques
.theme_rk <- function() {
  ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position  = "top",
      plot.title       = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}
