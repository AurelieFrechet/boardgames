library(ggplot2)

# Load dataset ------------------------------------------------------------

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

# On fixe le nombre de joueurs à 20 lorsqu'il ne sont pas renseignés
boardgames[is.na(boardgames$max_players),]
boardgames$max_players <- ifelse(is.na(boardgames$max_players), 20, boardgames$max_players)

# On retire les jeux dont la durée de jeu est 0 5extensions de jeux)
boardgames[boardgames$game_duration_minutes == 0, ]
boardgames <- boardgames[boardgames$game_duration_minutes > 0, ]

# On regroupe les catégories "Puzzle Adulte" et "Jeux Experts" car elle ne contient qu'une modalité
boardgames[boardgames$category == "Puzzle Adulte", ]
boardgames[boardgames$category == "Puzzle Adulte", ]$category <- "Jeux Experts"
boardgames$category <- factor(x = boardgames$category,
                              levels = c("Jeux Enfants", "Jeux Famille", "Jeux Initiés", "Jeux Experts"),
                              ordered = T)

### Range de joueureuses ----
boardgames$range_players <- paste(boardgames$min_players, boardgames$max_players, sep = "-")
table(boardgames$range_players)

### Peux-t-on joueur avec X Joueureuses
boardgames$J1 <- ifelse(boardgames$min_players == 1, T, F)
boardgames$J2 <- ifelse(boardgames$min_players <= 2 & boardgames$max_players >= 2, T, F)
boardgames$J3 <- ifelse(boardgames$min_players <= 3 & boardgames$max_players >= 3, T, F)
boardgames$J4 <- ifelse(boardgames$min_players <= 4 & boardgames$max_players >= 4, T, F)
boardgames$J5 <- ifelse(boardgames$min_players <= 5 & boardgames$max_players >= 5, T, F)
boardgames$J6 <- ifelse(boardgames$min_players <= 6 & boardgames$max_players >= 6, T, F)
boardgames$JPlus <- ifelse(boardgames$min_players <= 7 & boardgames$max_players >= 7, T, F)

str(boardgames)
summary(boardgames)

# Croisement des variables ------------------------------------------------


## Par catégorie de jeu --------------------------------------------------

###Game duration ----
qplot(game_duration_minutes, data = boardgames) +
  scale_x_continuous(breaks = c(0, 10, 30, 45, 60, 90, 120, 180),
                     limits = c(0, NA),
                     expand = c(0, 0))

ggplot(data = boardgames, aes(game_duration_minutes, category)) + 
  geom_violin() +
  # geom_boxplot() +
  scale_x_continuous(breaks = c(0, 10, 30, 45, 60, 90, 120, 180),
                     limits = c(0, NA),
                     expand = c(0, 0)) +
  theme_minimal()

### Age min ----
qplot(min_age, data = boardgames) 

ggplot(data = boardgames, aes(min_age, category)) + 
  geom_violin() +
  # geom_boxplot() +
  scale_x_continuous(limits = c(0, NA),
                     expand = c(0, 0)) +
  theme_minimal()

### Nombre de joueureuses ----

qplot(max_players, data = boardgames) 

ggplot(data = boardgames, aes(max_players, category)) + 
  # geom_violin() +
  geom_boxplot() +
  scale_x_continuous(limits = c(0, NA),
                     expand = c(0, 0)) +
  theme_minimal()



# ACM ---------------------------------------------------------------------

library(FactoMineR)
library(Factoshiny)

str(boardgames)
res <- FAMD(boardgames, sup.var = c(1, 6, 8))

coords <- res$ind$coord
boardgames_map <- cbind(boardgames, coords)


