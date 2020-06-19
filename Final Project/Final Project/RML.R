modelTrain <- function()
{
  dataset <- read.csv("~/adult_data.csv", header = FALSE)
  
  colnames(dataset) <- c("age","workclass","fnlwgt","education","education.num"
                         ,"marital.status","occupation","relationship","race","sex","capital.gain",
                         "capital.loss","hours.per.week","native.country","pay")
  
  control <- trainControl(method="cv", number=10)
  
  metric <- "Accuracy"

  # 選擇特徵
  dataset <- dataset[,c(1, 4:8, 11:13, 15)]

  set.seed(123)
  print('Start LDA')
  fit.lda <- train(pay~., data=dataset, method="lda", metric=metric, trControl=control)
  
  colnames(dataset) <- make.names(colnames(dataset))
  print('Start CART')
  fit.cart <- train(pay~., data=dataset, method="rpart", metric=metric, trControl=control)
  

  # summarize accuracy of models
  results <- resamples(list(lda=fit.lda, cart=fit.cart))
  print(summary(results))
  
  print('save model...')
  saveRDS(fit.lda, 'modelLDA.rds')
  saveRDS(fit.cart, 'modelCART.rds')
  print('model saved')
}