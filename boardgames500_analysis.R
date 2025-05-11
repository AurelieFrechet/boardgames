library(ggplot2)

# Load dataset ------------------------------------------------------------

## Chargement des données ----
boardgames <- read.csv(
  file = "data/bg_extract.csv",
  header = FALSE,
  col.names = c(
    "name",
    "min_age",
    "min_players",
    "max_players",
    "game_duration_minutes",
    "editor",
    "category"
  ),
  colClasses = c("character",
                 "numeric",
                 "numeric",
                 "numeric",
                 "numeric",
                 "factor",
                 "factor"),
  sep = ",",
  encoding = "UTF-8"
)

str(boardgames)
summary(boardgames)

## Transformation des variables ----

# On fixe le nombre de joueurs à 20 lorsqu'il ne sont pas renseignés
boardgames[is.na(boardgames$max_players),]
boardgames$max_players <- ifelse(is.na(boardgames$max_players), 20, boardgames$max_players)

# On retire les jeux dont la durée de jeu est 0 5extensions de jeux)
boardgames[boardgames$game_duration_minutes == 0, ]
boardgames <- boardgames[boardgames$game_duration_minutes > 0, ]

# On regroupe les catégories "Puzzle Adulte" et "Jeux Experts" car elle ne contient qu'une modalité
boardgames[boardgames$category == "Puzzle Adulte", ]$category <- "Jeux Experts"
boardgames$category <- factor(x = boardgames$category,
                              levels = c("Jeux Enfants", "Jeux Famille", "Jeux Initiés", "Jeux Experts"),
                              ordered = T)

#### Range de joueureuses 
# boardgames$range_players <- paste(boardgames$min_players, boardgames$max_players, sep = "-")
# table(boardgames$range_players)
# 
#### Peux-t-on joueur avec X Joueureuses
# boardgames$J1 <- ifelse(boardgames$min_players == 1, T, F)
# boardgames$J2 <- ifelse(boardgames$min_players <= 2 & boardgames$max_players >= 2, T, F)
# boardgames$J3 <- ifelse(boardgames$min_players <= 3 & boardgames$max_players >= 3, T, F)
# boardgames$J4 <- ifelse(boardgames$min_players <= 4 & boardgames$max_players >= 4, T, F)
# boardgames$J5 <- ifelse(boardgames$min_players <= 5 & boardgames$max_players >= 5, T, F)
# boardgames$J6 <- ifelse(boardgames$min_players <= 6 & boardgames$max_players >= 6, T, F)
# boardgames$JPlus <- ifelse(boardgames$min_players <= 7 & boardgames$max_players >= 7, T, F)

str(boardgames)

## Création de variables utiles ----
# Palette douce inspirée des petits chevaux
boardgames_palette <- c(
  "Jeux Experts" = "#FF3000",  
  "Jeux Initiés" = "#2FCDCD",  
  "Jeux Famille" = "#FFCD00",  
  "Jeux Enfants" = "#9ACD00"  
)

# Analyse descriptive -----------------------------------------------------
summary(boardgames)

## Catégorie de jeu
table(boardgames$category)

## Editeur
table(boardgames$category)

## Durée de jeu
ggplot(boardgames, aes(x=game_duration_minutes)) +
  geom_histogram()+
  scale_x_continuous(breaks = c(0, 10, 30, 45, 60, 90, 120, 180),
                     limits = c(0, NA),
                     expand = c(0, 0)) + 
  labs(title = "Temps de partie") + 
  xlab("Temps je jeu (en minutes)") +
  ylab("Nombre de jeux")

## Age min

## Nb de joueurs min

## Nb de joueurs max

# Croisement des variables ------------------------------------------------

### Category  x Game duration ----

ggplot(data = boardgames,
       aes(x = game_duration_minutes, y = category, fill = category)) +
  geom_violin(color = "transparent") +
  # geom_boxplot() +
  scale_x_continuous(
    name = "Temps de jeu",
    breaks = c(0, 10, 30, 45, 60, 90, 120, 180),
    limits = c(0, NA),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = boardgames_palette) +
  ylab("") +
  guides(fill = "none") +
  theme_minimal()

### Category x Age min ----
qplot(min_age, data = boardgames) 

ggplot(data = boardgames, aes(x = min_age, y = category, fill = category)) + 
  geom_violin(color = "transparent") +
  # geom_boxplot() +
  scale_x_continuous(limits = c(0, NA),
                     expand = c(0, 0)) +
  scale_fill_manual(values = boardgames_palette) +
  theme_minimal()

### Category x Nombre de joueureuses ----

qplot(max_players, data = boardgames) 

ggplot(data = boardgames, aes(max_players, category)) + 
  # geom_violin() +
  geom_boxplot() +
  scale_x_continuous(limits = c(0, NA),
                     expand = c(0, 0)) +
  theme_minimal()


### Category x Editor ----
table(boardgames$editor, boardgames$category)

### Editor x Game duration ----
ggplot(data = boardgames, aes(game_duration_minutes, editor)) + 
  geom_violin() +
  # geom_boxplot() +
  scale_x_continuous(breaks = c(0, 10, 30, 45, 60, 90, 120, 180),
                     limits = c(0, NA),
                     expand = c(0, 0)) +
  theme_minimal()

### Numerics ----
pairs(boardgames[, c("min_age", "min_players", "max_players", "game_duration_minutes")])




# ACM ---------------------------------------------------------------------

library(FactoMineR)
library(Factoshiny)

str(boardgames)
res <- FAMD(boardgames, sup.var = c(1, 6))

coords <- res$ind$coord
boardgames_map <- cbind(boardgames, coords)

saveRDS(boardgames_map, "data/boardgames_map.RDS")



library(plotly)
hover_text <- paste(
  "<b>", boardgames_map$name, "</b><br>",
  "Catégorie: ", boardgames_map$category, "<br>",
  "Durée: ", boardgames_map$game_duration_minutes, " min<br>",
  "Joueurs: ", paste(boardgames_map$min_players, boardgames_map$max_players, sep ="-")
)

plot_ly(
  data = boardgames_map,
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
