


# Load  & formating data ---------------------------------------------------------------

# Source: https://www.kaggle.com/datasets/jvanelteren/boardgamegeek-reviews/data
boardgame_reviews <- fread("data/bgg-26m-reviews.csv", drop = c("V1", "comment"))

boardgame_reviews[, user_nrates := .N, by = user]
boardgame_reviews[, median_game_rate := median(rating, na.rm = T), by = name]

# Top 5 games for each user
selected_users <- unique(boardgame_reviews[user_nrates >= 10 & rating >= median_game_rate & !is.na(name)]$user)
top_n_values <- boardgame_reviews[user %in% selected_users, 
                                  .SD[order(-rating)][1:5], 
                                  by = user]

# COmbinaison 2 games for each user
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

# Network plot ------------------------------------------------------------

if (!require(igraph)) install.packages("igraph")
library(igraph)

if (!require(networkD3)) install.packages("networkD3")
library(networkD3)

p <- simpleNetwork(bgr_network)
p
