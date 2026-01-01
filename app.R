library(shiny)
library(ggplot2)
library(dplyr)

# Fonction pour calculer l'évolution de l'investissement
calculer_investissement <- function(montant_depart, épargne_mensuelle, performance_annuelle, annees) {
  taux_mensuel <- (1 + performance_annuelle / 100)^(1/12) - 1
  valeur_investissement <- numeric(annees * 12 + 1)
  valeur_investissement[1] <- montant_depart
  apports_cumules <- numeric(annees * 12 + 1)
  apports_cumules[1] <- montant_depart

  for (mois in 1:(annees * 12)) {
    valeur_investissement[mois + 1] <- (valeur_investissement[mois] + épargne_mensuelle) * (1 + taux_mensuel)
    apports_cumules[mois + 1] <- apports_cumules[mois] + épargne_mensuelle
  }

  interets_cumules <- valeur_investissement - apports_cumules

  data.frame(
    Temps = seq(0, annees, length.out = length(valeur_investissement)),
    Investissement = valeur_investissement,
    Interets = interets_cumules
  )
}

# Interface utilisateur (UI)
ui <- fluidPage(
  titlePanel("Simulateur d'investissement"),
  sidebarLayout(
    sidebarPanel(
      numericInput("montant_depart", "Montant de départ (€) :", value = 10000, min = 0),
      numericInput("epargne_mensuelle", "Épargne mensuelle (€) :", value = 500, min = 0),
      numericInput("performance", "Performance annuelle (%) :", value = 5, min = 0),
      sliderInput("annees", "Durée de l'investissement (années) :", min = 1, max = 30, value = 10),
      actionButton("simuler", "Lancer la simulation")
    ),
    mainPanel(
      plotOutput("graphique_investissement"),
      h4("Valeur finale de l'investissement :"),
      verbatimTextOutput("valeur_finale"),
      h4("Intérêts cumulés finaux :"),
      verbatimTextOutput("interets_finaux")
    )
  )
)

# Logique de l'application (Server)
server <- function(input, output) {
  simulation_data <- eventReactive(input$simuler, {
    calculer_investissement(
      montant_depart = input$montant_depart,
      épargne_mensuelle = input$epargne_mensuelle,
      performance_annuelle = input$performance,
      annees = input$annees
    )
  })

  output$graphique_investissement <- renderPlot({
    df <- simulation_data()
    ggplot(df, aes(x = Temps)) +
      geom_line(aes(y = Investissement, color = "Valeur totale"), size = 1.2) +
      geom_line(aes(y = Interets, color = "Intérêts cumulés"), size = 1.2) +
      labs(
        title = "Évolution de l'investissement et des intérêts cumulés",
        x = "Années",
        y = "Valeur (€)",
        color = "Légende"
      ) +
      scale_color_manual(values = c("Valeur totale" = "blue", "Intérêts cumulés" = "orange")) +
      theme_minimal() +
      theme(legend.position = "top")
  })

  output$valeur_finale <- renderPrint({
    df <- simulation_data()
    cat("€", round(df$Investissement[length(df$Investissement)], 2))
  })

  output$interets_finaux <- renderPrint({
    df <- simulation_data()
    cat("€", round(df$Interets[length(df$Interets)], 2))
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
