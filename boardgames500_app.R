# Packages & data ----------------------------------------------------------
library(shiny)
library(bslib)
library(plotly)

# Chargement des données 
boardgames <- readRDS("data/boardgames_map.RDS")

# Thèmes
boardgames_palette <- c(
  "Jeux Experts" = "#FF3000",  
  "Jeux Initiés" = "#2FCDCD",  
  "Jeux Famille" = "#FFCD00",  
  "Jeux Enfants" = "#9ACD00"  
)

boardgames_theme <- bs_theme(
  version = 5,
  bg = "#F9F9F9",
  fg = "#222222",
  primary = "#FF3000",  
  secondary = "#9ACD00", 
  success = "#FFCD00",  
  info = "#2FCDCD"
)


# UI ----------------------------------------------------------------------
ui <- page_fluid(
  theme = boardgames_theme,
  
  titlePanel("Jeux de société - Sélection personnalisée"),
  
  layout_sidebar(
    sidebar = sidebar(
      selectInput(
        "category",
        "Catégorie de jeu",
        choices  = unique(boardgames$category),
        selected = unique(boardgames$category),
        multiple = TRUE
      ),
      sliderInput(
        "min_age",
        "Âge minimum",
        min = min(boardgames$min_age),
        max = max(boardgames$min_age),
        value = 8
      ),
      sliderInput(
        "game_duration_minutes",
        "Durée de jeu (minutes)",
        min = min(boardgames$game_duration_minutes),
        max = max(boardgames$game_duration_minutes),
        step = 15,
        value = c(5, 30)
      ),
      sliderInput(
        "nb_players",
        "Nombre de joueurs",
        min = min(boardgames$min_players),
        max = max(boardgames$max_players),
        value = c(2, 4)
      )
    ),
    
    plotlyOutput("scatterPlot")
    
  )
)


# Server ------------------------------------------------------------------


server <- function(input, output) {
  filtered_data <- reactive({
    boardgames[boardgames$category %in% input$category &
                 boardgames$min_age >= input$min_age &
                 boardgames$game_duration_minutes >= input$game_duration_minutes[1] &
                 boardgames$game_duration_minutes <= input$game_duration_minutes[2] &
                 boardgames$min_players >= input$nb_players[1] &
                 boardgames$max_players <= input$nb_players[2], ]
  })
  
  output$scatterPlot <- renderPlotly({
    hover_text <- paste(
      "<b>", filtered_data()$name, "</b><br>",
      "Catégorie: ", filtered_data()$category, "<br>",
      "Durée: ", filtered_data()$game_duration_minutes, " min<br>",
      "Joueurs: ", paste(filtered_data()$min_players, filtered_data()$max_players, sep ="-")
    )
    
    
    plot_ly(
      data = filtered_data(),
      x = ~Dim.1,
      y = ~Dim.2,
      type = 'scatter',
      mode = 'markers',
      color = ~category,
      colors = boardgames_palette,
      text = hover_text,
      hoverinfo = "text",
      marker = list(size = 10, opacity = 0.85)
    ) %>%
      layout(
        xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        plot_bgcolor = "#FAFAFA",
        paper_bgcolor = "#FAFAFA",
        legend = list(title = list(text = "Catégorie"))
      )
  })
}


# App ---------------------------------------------------------------------


shinyApp(ui, server)
