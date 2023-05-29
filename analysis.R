library(dplyr)


# read data
setwd('~/Desktop/songs')
df <-  read.csv('analysisData.csv')
head(df)


# get performer percent rank
performer_freq <- table(df$performer) %>% 
  as.data.frame() %>%
  rename(performer = Var1, freq = Freq) %>%
  arrange(desc(freq))
performer_freq$performer_rank <- percent_rank(performer_freq$freq)
performer_freq = performer_freq %>% select(performer, performer_rank)


# genre dummies
genre_vector <- gsub("\\[|\\]|'", '', df$genre)
genre_list <- strsplit(genre_vector, ', ') %>%
  lapply(function(x) {
    x <- gsub('hip hop', 'hip-hop', x)
    return(strsplit(x, ' ') %>% unlist())
  })

genre_freq <- genre_list %>% 
  unlist() %>% 
  table() %>% 
  as.data.frame()


colnames(genre_freq) <- c('genre', 'freq')
genre_freq <- arrange(genre_freq, desc(freq))
hist(genre_freq$freq)


selected_genre <- genre_freq %>% 
  filter(freq >= 1000) %>%
  select(genre)


genre_dummies <- data.frame()
for (genre in selected_genre$genre) {
  genre_dummy <- sapply(genre_list, function(x) {ifelse(genre %in% x, 1, 0)})
  genre_dummy <- data.frame(genre_dummy)
  colnames(genre_dummy) <- c(genre)
  if (length(genre_dummies) == 0) {
    genre_dummies <- genre_dummy
  } else {
    genre_dummies <- cbind(genre_dummies, genre_dummy)
  }
}


# prepare linear regression data
reg_df <- df[, -c(1, 3, 4)]
reg_df <- merge(reg_df, performer_freq)
reg_df <- cbind(reg_df, genre_dummies)
reg_df$track_explicit <- as.integer(reg_df$track_explicit)
reg_df <- reg_df[, -1] 


# fit linear regression model
model <- lm(rating ~ ., reg_df)
summary(model)



