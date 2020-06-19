predictionTest <- function()
{
  dataset <- read.csv("~/Final/Dataset/adult_test.csv", header = FALSE)
  
  colnames(dataset) <- c("age","workclass","fnlwgt","education","education.num"
                         ,"marital.status","occupation","relationship","race","sex","capital.gain",
                         "capital.loss","hours.per.week","native.country","pay")

  # 選擇特徵
  dataset <- dataset[,c(1, 4:8, 11:13, 15)]
  
  # 載入訓練模型 (前9特徵: model1, 最高9特徵: model2, 15特徵: model3)
  fit.lda <- readRDS('~/Final/Model/model2LDA.rds')
  fit.cart <- readRDS('~/Final/Model/model2CART.rds')
  
  print("LDA Result")
  predictionsLDA <- predict(fit.lda, dataset)
  print(confusionMatrix(predictionsLDA, dataset$pay))
  
  print("cart Result")
  predictionsCART <- predict(fit.cart, dataset)
  print(confusionMatrix(predictionsCART, dataset$pay))
  
}