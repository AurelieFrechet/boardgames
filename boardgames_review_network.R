# Packages ----------------------------------------------------------------

if (!require(data.table)) install.packages("data.table")
library(data.table)


# Load  & formating data ---------------------------------------------------------------

# Source: https://www.kaggle.com/datasets/jvanelteren/boardgamegeek-reviews/data
boardgame_reviews <- fread("data/bgg-26m-reviews.csv", drop = c("V1", "comment"))

boardgame_reviews[, user_nrates := .N, by = user]
boardgame_reviews[, median_game_rate := median(rating, na.rm = T), by = name]

# Top 5 games for each user
top_nb <- 3
selected_users <- unique(boardgame_reviews[user_nrates >= 10 & rating >= median_game_rate & !is.na(name)]$user)
top_n_values <- boardgame_reviews[user %in% selected_users, 
                                  .SD[order(-rating)][1:top_nb], 
                                  by = user]

# Combinaison 2 games for each user
bgr_split <- split(top_n_values, top_n_values$user)
bgr_lapply <- lapply(bgr_split, function(user_x){
  games <- sort(user_x$name)
  combn(games, m = 2) |> t()
})

bgr_bind <- do.call(rbind, bgr_lapply) |> as.data.table()
colnames(bgr_bind) <- c("game1", "game2")

# Data.table source-target (network)
bgr_network <- bgr_bind[, .(n = .N), by = c("game1", "game2")]

str(bgr_network)
summary(bgr_network)

bgr_network <- bgr_network[n > 100]
hist(log10(bgr_network$n))

# Network plot ------------------------------------------------------------

if (!require(igraph)) install.packages("igraph")
library(igraph)

if (!require(networkD3)) install.packages("networkD3")
library(networkD3)

if (!require(ggraph)) install.packages("ggraph")
library(ggraph)


## Simple Network
# -> Confusing and unreadible
p <- simpleNetwork(bgr_network,
                   charge = -500,
                   fontSize = 3)
p


## Force Network


info_nodes <- boardgame_reviews[, .(n_rates = .N, mean_rate = round(mean(rating, na.rm = T), 0)), by = name]


str(bgr_network)
str(info_nodes)
# Plot
p_force <- forceNetwork(
  Links = bgr_network,
  Nodes = info_nodes,
  Source = "game1",
  Target = "game2",
  Value = "n",
  NodeID = "name",
  Nodesize = "n_rates",
  Group = "mean_rate",
  charge = -500,
  opacity = 1,
  legend = TRUE,
  zoom = TRUE
)

p_force


