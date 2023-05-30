# load data
analysis <- read.csv('analysisData.csv')
scoring <- read.csv('scoringData.csv')

# calculate performer frequency rank
performer_freq <- table(analysis$performer) %>% data.frame() %>% arrange(desc(Freq)) %>%
  mutate(`Percent Rank` = percent_rank(Freq)) %>% rename(Performer = Var1)
head(performer_freq) %>% kable(caption = 'Performer percentage rank')

# create genre pool
analysis_genre <- gsub("\\[|\\]|'", '', analysis$genre) %>% strsplit(', ') 
scoring_genre <- gsub("\\[|\\]|'", '', scoring$genre) %>% strsplit(', ')
genre_pool <- intersect(unlist(analysis_genre), unlist(scoring_genre)) %>% unique()

# loop through all shared genre
for (genre in genre_pool) {
  analysis_dummy <- analysis_genre %>% 
    sapply(function(genres) {genre %in% genres}) %>% as.integer()
  analysis <- analysis %>% cbind(analysis_dummy)
  scoring_dummy <- scoring_genre %>% 
    sapply(function(genres) {genre %in% genres}) %>% as.integer()
  scoring <- scoring %>% cbind(scoring_dummy)
}

# create performer pool
analysis_performer <- analysis$performer %>% strsplit(' Featuring |, | & ')
scoring_performer <- scoring$performer %>% strsplit(' Featuring |, | & ')
performer_pool <- intersect(unlist(analysis_performer), unlist(scoring_performer)) %>% unique()

# loop through all shared genre
for (performer in performer_pool) {
  analysis_dummy <- analysis_performer %>% 
    sapply(function(performers) {performer %in% performers}) %>% as.integer()
  analysis <- analysis %>% cbind(analysis_dummy)
  scoring_dummy <- scoring_performer %>% 
    sapply(function(performers) {performer %in% performers}) %>% as.integer()
  scoring <- scoring %>% cbind(scoring_dummy)
}

# convert logical variables to numeric
analysis$track_explicit <- as.integer(analysis$track_explicit)
scoring$track_explicit <- as.integer(scoring$track_explicit)

# delete id, performer, genre, song
analysis <- analysis[, -c(1:4)]
scoring <- scoring[, -c(1:4)]

set.seed(1031)
train_index <- createDataPartition(analysis$rating, p = .8, list = F)
train <- analysis[train_index,]
test <- analysis[-train_index,]

train_x <- train[, -15] 
train_y <- train[, 15]

test_x <- test[, -15]
test_y <- test[, 15]

# fit and test the linear model
linear_model <- lm(rating ~ ., data = train)
y_pred <- predict(linear_model, test)

RMSE(y_pred, test_y)

# build train and test set for xgboost model
xgb_train <- xgb.DMatrix(data = as.matrix(train_x), label = train_y)
xgb_test <- xgb.DMatrix(data = as.matrix(test_x), label = test_y)

# train xgboost model
watchlist <- list(train = xgb_train, test = xgb_test)
xgb_model <- xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, 
                       nrounds = 100, gamma = 0, eta = 0.3, verbose = F)

# test xgboost model
pred_y = predict(xgb_model, xgb_test)
caret::RMSE(test_y, pred_y)

# feature importance
importance_matrix <- xgb.importance(colnames(train_x), model = xgb_model)

# choose top features
selected_features <- importance_matrix$Feature[0:200]
select_cols <- match(selected_features, colnames(train_x))

# new dataset with selected columns
train_x_selected <- train_x[, select_cols]
test_x_selected <- test_x[, select_cols]

# build train and test set for the selected xgboost model
xgb_train_selected <- xgb.DMatrix(data = as.matrix(train_x_selected), label = train_y)
xgb_test_selected <- xgb.DMatrix(data = as.matrix(test_x_selected), label = test_y)

# train xgboost model again
watchlist <- list(train = xgb_train_selected, test = xgb_test_selected)
xgb_model_selected <- xgb.train(data = xgb_train_selected, 
                                max.depth = 4, watchlist=watchlist, 
                                nrounds = 500, gamma = 0, eta = 0.1, verbose = F)

# show the test result
pred_y = predict(xgb_model_selected, xgb_test_selected)
caret::RMSE(test_y, pred_y)

# Use selected feature and train again
xgb_x <- data.matrix(analysis[, -15][, select_cols])
xgb_y <- data.matrix(analysis[, 15])
xgb_data <- xgb.DMatrix(data = xgb_x, label = xgb_y)
xgb_model <- xgboost(data = xgb_data, max.depth = 4, 
                     nrounds = 500, gamma = 0, eta = 0.1, verbose = F)
# make prediction on scoring data
scoring_x = scoring[, select_cols]
colnames(scoring_x) = gsub('scoring', 'analysis', colnames(scoring_x))
scoring_x = xgb.DMatrix(as.matrix(scoring_x))
prediction = predict(xgb_model, scoring_x) %>% as.data.frame()
colnames(prediction) <- 'prediction'
scoring_id = read.csv('scoringData.csv')$id
result <- data.frame(id = scoring_id, rating = prediction$predict)
head(result, 10) %>% kable(captian = 'Submission Sample')