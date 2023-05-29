# The-perfect-tune

Description
What makes some songs become popular? The dataset describes about 20,000 popular songs based on auditory features such as loudness and tempo.*
Goal
Construct a model using a dataset of popular songs to predict rating based on auditory features of the songs included in scoringData.csv.
Metric
Submissions will be evaluated based on RMSE (root mean squared error) (Wikipedia). Lower the RMSE, better the model. Read data and construct a simple model
songs = read.csv('analysisData.csv')
model = lm(rating~ tempo+time_signature,songs)
Read in scoring data and apply model to generate predictions
scoringData = read.csv('scoringData.csv')
pred = predict(model,newdata=scoringData)
Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, rating = pred)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)
