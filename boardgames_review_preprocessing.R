

# Packages ----------------------------------------------------------------

if (!require(data.table)) install.packages("data.table")
library(data.table)

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

if (!require(ggExtra)) install.packages("ggExtra")
library(ggExtra)

if (!require(FactoMineR)) install.packages("FactoMineR")
library(FactoMineR)

# Data --------------------------------------------------------------------

# Source: https://www.kaggle.com/datasets/jvanelteren/boardgamegeek-reviews/data
boardgame_reviews <- fread("data/bgg-26m-reviews.csv", drop = c("V1", "comment"))

## Aggregate
users_agg <- boardgame_reviews[, .(n_rates = .N, 
                                   mean_rate = mean(rating, na.rm = T),
                                   median_rate = median(rating, na.rm = T),
                                   q75 = quantile(rating, probs = 0.75, na.rm = T)), 
                                 by = user]


games_agg <- boardgame_reviews[, .(n_rates = .N, 
                                   mean_rate = mean(rating, na.rm = T),
                                   median_rate = median(rating, na.rm = T)), 
                               by = name]



# Data Analysis -----------------------------------------------------------

## Full data ------------------------------------------------------------------
str(boardgame_reviews)
# Classes ‘data.table’ and 'data.frame':	26200012 obs. of  4 variables:
#   $ user  : chr  "sidehacker" "Varthlokkur" "dougthonus" "cypar7" ...
#   $ rating: num  10 10 10 10 10 10 10 10 10 10 ...
#   $ ID    : int  13 13 13 13 13 13 13 13 13 13 ...
#   $ name  : chr  "CATAN" "CATAN" "CATAN" "CATAN" ...


## Reviewer profile ------------------------------------------------------------
str(users_agg)
# Classes ‘data.table’ and 'data.frame':	555482 obs. of  5 variables:
#   $ user       : chr  "sidehacker" "Varthlokkur" "dougthonus" "cypar7" ...
#   $ n_rates    : int  11 76 4 1956 889 15 36 66 106 305 ...
#   $ mean_rate  : num  8.68 7.58 9.25 5.91 6.13 ...
#   $ median_rate: num  9 7 9.5 6 6 7 6.75 7 7 8 ...
#   $ q75        : num  9.25 9 10 8 7 8 8.25 8 8 8 ...

ggplot(users_agg, aes(x=mean_rate)) +
  geom_histogram(bins = 20)+
  expand_limits( x= c(1, 10)) +
  labs(title = "Mean rate given by reviewer")+
         xlab("Rating") +
         ylab("Number of Reviewers")

ggplot(users_agg, aes(x = n_rates)) +
  geom_histogram(bins = 40) + 
  scale_x_log10(name="Count",
                breaks = 10^(-10:10),
                minor_breaks = rep(1:9, 21)*(10^rep(-10:10, each=9))) +
  scale_y_continuous(name="Number of Reviewers") + 
  expand_limits( x= c(1, 10000)) +
  labs(title = "Number of rate given by reviewer") 


ggplot(users_agg, aes(x = n_rates, y = mean_rate)) +
  geom_bin2d()+ 
  geom_hline(yintercept = median(users_agg$mean_rate, na.rm = T), 
             linetype="dashed", color = "red")+
  geom_hline(yintercept = mean(users_agg$mean_rate, na.rm = T), color = "red")+
  scale_y_continuous(name="Mean Rate") +
  scale_x_log10(name="Number of Rates",
                breaks = 10^(-10:10),
                minor_breaks = rep(1:9, 21)*(10^rep(-10:10, each=9))) 
  


## Game profile ----------------------------------------------------------------
str(games_agg)
# Classes ‘data.table’ and 'data.frame':	27346 obs. of  4 variables:
#   $ name       : chr  "CATAN" "Carcassonne" "Pandemic" "7 Wonders" ...
#   $ n_rates    : int  131303 131205 128952 107527 103950 100041 99667 96517 96081 93027 ...
#   $ mean_rate  : num  7.14 7.41 7.53 7.67 8.35 ...
#   $ median_rate: num  7 7.4 7.84 8 8.5 ...

ggplot(games_agg, aes(x = mean_rate)) +
  geom_histogram() +
  labs(title = "Mean rate received by games") +
  xlab("Rating") +
  ylab("Number of Games")

ggplot(games_agg, aes(x = n_rates)) +
  geom_histogram(bins = 40) + 
  scale_x_log10(name="Count", limits = c(1, 10000)) +
  scale_y_continuous(name="Number of Games") +
  labs(title = "Number of rate received by games") 


ggplot(games_agg, aes(x = n_rates, y = mean_rate)) +
  geom_bin2d()+ 
  geom_hline(yintercept = median(games_agg$mean_rate, na.rm = T), 
             linetype="dashed", color = "red")+
  geom_hline(yintercept = mean(games_agg$mean_rate, na.rm = T), color = "red")+
  scale_y_continuous(name="Mean Rate") +
  scale_x_log10(name="Number of Rates",
                breaks = 10^(-10:10),
                minor_breaks = rep(1:9, 21)*(10^rep(-10:10, each=9))) 


# Modelisation ------------------------------------------------------------
# All users with only one rate
users_selected <- users_agg[n_rates>=10]

# Keep users with more than 10 rates
bg_reviews <- boardgame_reviews[users_selected, on = "user", nomatch = NULL]

## Idea 1: Network of Top 5 games by reviewer ------------------------------
top_n_values <- boardgame_reviews[, .SD[order(-rating)][1:5], by = user]

str(top_n_values)
length(unique(top_n_values$name))
summary(top_n_values)




## Idea 2: HIGH dimensional PCA on large matrix ----------------------------
n_users <- length(unique(bg_reviews$user))
n_games <- length(unique(bg_reviews$name))

# Random sample
test_subset <- bg_reviews[sample(x = 1:nrow(bg_reviews), size = 1000, replace = F)]
length(unique(test_subset$user))
length(unique(test_subset$name))

ggplot(bg_reviews, aes(name, user, fill= rating)) + 
  geom_tile()

# Smart sample (500 games with the most reviews)
#
n_best_games = 50
selected_games <- head(games_agg[order(n_rates, decreasing = T)], n_best_games)
subset_reviews <- boardgame_reviews[selected_games, on = "name", nomatch = NULL]

length(unique(subset_reviews$name))
length(unique(subset_reviews$user))

subset_users <- subset_reviews[, .(n_rates := .N), by = user]
max(subset_users$n_rates)

subset_reviews <- subset_reviews[subset_users, on = "user", nomatch = NULL]

ggplot(subset_reviews, aes(name, user, fill= rating)) + 
  geom_tile()

review_matrix <- tapply(subset_reviews$rating, subset_reviews[, c("name", "user")], mean)

PCA(X = review_matrix)

