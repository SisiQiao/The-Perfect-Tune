library(dplyr)
library('caret')


setwd('~/Desktop/songs')
df <- read.csv('analysisData.csv')


# performer
performer_freq <- table(df$performer) %>% as.data.frame() %>% arrange(desc(Freq))
performer_freq$performer_rank <- percent_rank(performer_freq$Freq)
performer_freq <- performer_freq %>% 
  rename(performer = Var1) %>% 
  select(performer, performer_rank)


# genre
genre_list <- gsub("\\[|\\]|'", '', df$genre) %>% strsplit(', ')
# genre_list <- genre_list %>% lapply(function(x) {strsplit(x, ' ') %>% unlist()})
genre_pool <- genre_list %>% unlist()
genre_freq <- table(genre_pool) %>% as.data.frame() %>% arrange(desc(Freq))
selected_genre <- genre_freq %>% 
  rename(genre = genre_pool) %>% 
  filter(Freq >= 1000) %>% 
  select(genre)


genre_dummies <- data.frame()
for (genre in selected_genre$genre) {
  genre_dummy <- sapply(genre_list, function(x) {ifelse(genre %in% x, 1, 0)})
  genre_dummy <- as.data.frame(genre_dummy)
  colnames(genre_dummy) <- genre
  if (length(genre_dummies) == 0) {
    genre_dummies <- genre_dummy
  } else {
    genre_dummies <- cbind(genre_dummies, genre_dummy)
  }
}


# prepare regression data
reg_df <- df[, -c(1, 3, 4)]
reg_df <- merge(reg_df, performer_freq)
reg_df <- cbind(reg_df, genre_dummies)
reg_df <- reg_df[, -1]
reg_df$track_explicit <- as.integer(reg_df$track_explicit)


# split into training and testing set
set.seed(617)
train_index <- createDataPartition(reg_df$rating, p = 0.8, groups = 10, list = FALSE)
train <- reg_df[train_index,]
test <- reg_df[-train_index,]


# fit linear regression model
model <- lm(rating ~ ., data = train)
summary(model)
model_selected <- step(model)
summary(model_selected)


# RMSE
rating_pred <- predict(model_selected, test)
rating_real <- test$rating
rmse <- sqrt(mean((rating_pred - rating_real)^2))
rmse


# load scoring data
scoring_df <- read.csv('scoringData.csv')


# performer
performer_freq <- table(scoring_df$performer) %>% as.data.frame() %>% arrange(desc(Freq))
performer_freq$performer_rank <- percent_rank(performer_freq$Freq)
performer_freq <- performer_freq %>% 
  rename(performer = Var1) %>% 
  select(performer, performer_rank)


# genre
genre_list <- gsub("\\[|\\]|'", '', scoring_df$genre) %>% strsplit(', ')
genre_dummies <- data.frame()
for (genre in selected_genre$genre) {
  genre_dummy <- sapply(genre_list, function(x) {ifelse(genre %in% x, 1, 0)})
  genre_dummy <- as.data.frame(genre_dummy)
  colnames(genre_dummy) <- genre
  if (length(genre_dummies) == 0) {
    genre_dummies <- genre_dummy
  } else {
    genre_dummies <- cbind(genre_dummies, genre_dummy)
  }
}


# prepare regression data
reg_df <- scoring_df[, -c(1, 3, 4)]
reg_df <- merge(reg_df, performer_freq)
reg_df <- cbind(reg_df, genre_dummies)
reg_df <- reg_df[, -1]
reg_df$track_explicit <- as.integer(reg_df$track_explicit)


# RMSE
rating_pred <- predict(model_selected, reg_df)
result <- data.frame(id = scoring_df$id, rating = rating_pred)
write.csv(result, 'submission.csv', row.names = F)
