library(shiny)
library(ggplot2)
library(rungekutta)

# ---------- Fonctions utilitaires ----------

# Simule avec une mesure qui réduit beta de `eff` à partir du jour `jour`
simuler <- function(beta, gamma, N, I0, duree, h, jour = Inf, eff = 0) {
  f_avant <- modele_sir(beta, gamma, N)
  f_apres <- modele_sir(beta * (1 - eff), gamma, N)
  f  <- function(t, y) if (t < jour) f_avant(t, y) else f_apres(t, y)
  y0 <- c(S = N - I0, I = I0, R = 0)
  rk4(f, y0, t0 = 0, tf = duree, h = h)
}

# Pic d'infectés, jour du pic, part touchée, jours au-dessus de la capacité
indic <- function(s, N, cap, h) {
  i <- which.max(s$I)
  c(round(max(s$I)), s$t[i], round(100 * tail(s$R, 1) / N, 1),
    round(sum(s$I > cap) * h))
}

# Réduction minimale de beta (en %) pour que le pic reste <= cap.
# Renvoie 0 si aucune mesure n'est nécessaire, NA si même 90 % ne suffit pas.
eff_min <- function(beta, gamma, N, I0, duree, h, jour, cap) {
  pic <- function(e) {
    max(simuler(beta, gamma, N, I0, duree, h, jour = jour, eff = e)$I)
  }
  if (pic(0) <= cap)  return(0)
  if (pic(0.9) > cap) return(NA_real_)
  bas <- 0; haut <- 0.9
  while (haut - bas > 0.005) {
    milieu <- (bas + haut) / 2
    if (pic(milieu) > cap) bas <- milieu else haut <- milieu
  }
  ceiling(haut * 100)
}

# Format long pour l'export CSV
tab_long <- function(s, nom) {
  data.frame(t = s$t, S = s$S, I = s$I, R = s$R, Scenario = nom)
}

# ---------- Interface ----------

ui <- fluidPage(
  titlePanel("Simulateur d'épidémie SIR : aide à la décision"),
  sidebarLayout(
    sidebarPanel(
      actionButton("douala", "Scénario Douala", class = "btn-primary"),
      hr(),
      h4("Épidémie"),
      numericInput("N",  "Population N", 10000, min = 1),
      numericInput("I0", "Infectés initiaux", 10, min = 1),
      sliderInput("beta",  "Transmission β", 0.05, 1,   0.3, step = 0.01),
      sliderInput("gamma", "Guérison γ",     0.02, 0.5, 0.1, step = 0.01),
      sliderInput("duree", "Durée (jours)",  30, 365, 160),
      sliderInput("h",     "Pas h",          0.05, 1, 0.5, step = 0.05),
      h4("Mesure de prévention"),
      sliderInput("jour", "Début de la mesure (jour)", 0, 200, 30),
      sliderInput("eff",  "Réduction de β (%)", 0, 90, 40),
      h4("Système de santé"),
      numericInput("cap", "Capacité (infectés simultanés)", 1000, min = 0)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Comparaison",
                 br(),
                 textOutput("resume"),
                 strong(textOutput("effmin")),
                 br(),
                 tableOutput("indicateurs"),
                 plotOutput("courbes"),
                 downloadButton("dl_png", "Télécharger le graphique (PNG)"),
                 downloadButton("dl_csv", "Télécharger les résultats (CSV)")
        ),
        tabPanel("S, I, R",
                 br(),
                 radioButtons("scen", "Scénario",
                              c("Avec mesure" = "avec", "Sans mesure" = "sans"),
                              inline = TRUE),
                 textOutput("verif"),
                 plotOutput("sir")
        ),
        tabPanel("Coût du retard",
                 br(),
                 helpText("Pic d'infectés selon le jour de début de la mesure,",
                          "avec la réduction de β choisie à gauche."),
                 plotOutput("retard")
        )
      )
    )
  )
)

# ---------- Serveur ----------

server <- function(input, output, session) {

  sans <- reactive({
    validate(need(input$I0 < input$N, "Il faut I0 < N."))
    simuler(input$beta, input$gamma, input$N, input$I0,
            input$duree, input$h)
  })

  avec <- reactive({
    simuler(input$beta, input$gamma, input$N, input$I0, input$duree,
            input$h, jour = input$jour, eff = input$eff / 100)
  })

  effmin <- reactive({
    sans()  # déclenche la validation (I0 < N)
    eff_min(input$beta, input$gamma, input$N, input$I0,
            input$duree, input$h, input$jour, input$cap)
  })

  retard <- reactive({
    sans()
    jours <- seq(0, min(90, input$duree), by = 10)
    pics <- sapply(jours, function(j) {
      max(simuler(input$beta, input$gamma, input$N, input$I0, input$duree,
                  input$h, jour = j, eff = input$eff / 100)$I)
    })
    data.frame(jour = jours, pic = pics)
  })

  graphique <- reactive({
    s <- sans(); a <- avec()
    d <- rbind(
      data.frame(t = s$t, I = s$I, Scenario = "Sans mesure"),
      data.frame(t = a$t, I = a$I, Scenario = "Avec mesure")
    )
    ggplot(d, aes(t, I, colour = Scenario, linetype = Scenario)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = input$jour, linetype = "dotted") +
      geom_hline(yintercept = input$cap, colour = "red") +
      annotate("text", x = 0, y = input$cap, label = "Capacité",
               colour = "red", vjust = -0.5, hjust = 0) +
      labs(x = "Jours", y = "Infectés") +
      theme_minimal()
  })

  # --- Onglet Comparaison ---

  output$resume <- renderText({
    r0_apres <- input$beta * (1 - input$eff / 100) / input$gamma
    evites <- round(tail(sans()$R, 1) - tail(avec()$R, 1))
    paste0("R0 sans mesure : ", round(input$beta / input$gamma, 2),
           " | R0 après mesure : ", round(r0_apres, 2),
           " | Infections évitées : ", evites)
  })

  output$effmin <- renderText({
    e <- effmin()
    if (is.na(e)) {
      "Même une réduction de 90 % ne suffit pas : la capacité est dépassée avant ou malgré la mesure."
    } else if (e == 0) {
      "Aucune mesure nécessaire : la capacité n'est jamais dépassée."
    } else {
      paste0("Réduction minimale de β pour rester sous la capacité : ",
             e, " % (mesure au jour ", input$jour, ")")
    }
  })

  output$indicateurs <- renderTable({
    data.frame(
      Indicateur = c("Pic d'infectés", "Jour du pic", "Part touchée (%)",
                     "Jours au-dessus de la capacité"),
      `Sans mesure` = indic(sans(), input$N, input$cap, input$h),
      `Avec mesure` = indic(avec(), input$N, input$cap, input$h),
      check.names = FALSE
    )
  })

  output$courbes <- renderPlot(graphique())

  output$dl_png <- downloadHandler(
    filename = function() "comparaison_scenarios.png",
    content = function(file) {
      ggsave(file, graphique(), device = "png",
             width = 9, height = 5, dpi = 150)
    }
  )

  output$dl_csv <- downloadHandler(
    filename = function() "resultats_sir.csv",
    content = function(file) {
      d <- rbind(tab_long(sans(), "Sans mesure"),
                 tab_long(avec(), "Avec mesure"))
      write.csv(d, file, row.names = FALSE)
    }
  )

  # --- Onglet S, I, R ---

  solution_choisie <- reactive({
    if (input$scen == "avec") avec() else sans()
  })

  output$verif <- renderText({
    s <- solution_choisie()
    ecart <- max(abs(s$S + s$I + s$R - input$N))
    paste0("Vérification : écart maximal |S + I + R − N| = ",
           format(ecart, scientific = TRUE, digits = 3), " personne(s)")
  })

  output$sir <- renderPlot({
    s <- solution_choisie()
    d <- data.frame(t = rep(s$t, 3),
                    Groupe = rep(c("S", "I", "R"), each = nrow(s)),
                    n = c(s$S, s$I, s$R))
    ggplot(d, aes(t, n, colour = Groupe)) +
      geom_line(linewidth = 1) +
      labs(x = "Jours", y = "Personnes") +
      theme_minimal()
  })

  # --- Onglet Coût du retard ---

  output$retard <- renderPlot({
    ggplot(retard(), aes(jour, pic)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      geom_hline(yintercept = input$cap, colour = "red") +
      labs(x = "Début de la mesure (jour)", y = "Pic d'infectés",
           title = paste0("Réduction de β de ", input$eff, " %")) +
      theme_minimal()
  })

  # --- Scénario Douala ---

  observeEvent(input$douala, {
    updateNumericInput(session, "N",  value = 3800000)
    updateNumericInput(session, "I0", value = 100)
    updateSliderInput(session, "beta",  value = 0.35)
    updateSliderInput(session, "gamma", value = 0.14)
    updateSliderInput(session, "duree", value = 250)
    updateSliderInput(session, "h",     value = 0.5)
    updateSliderInput(session, "jour",  value = 30)
    updateSliderInput(session, "eff",   value = 40)
    updateNumericInput(session, "cap",  value = 100000)
    showNotification("Scénario Douala chargé", type = "message")
  })
}

shinyApp(ui, server)
