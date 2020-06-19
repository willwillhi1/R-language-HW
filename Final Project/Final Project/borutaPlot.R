borutaPlot <- function()
{
  data <- read.csv("~/adult_data.csv", header = FALSE)
  
  colnames(data) <- c("age","workclass","fnlwgt","education","education.num"
                         ,"marital.status","occupation","relationship","race","sex","capital.gain",
                         "capital.loss","hours.per.week","native.country","income")
  
  # 處理缺漏資料
  data[data == " ?"] <- NA
  data <- data[complete.cases(data),]
  
  # 非線性特徵資料處理
  convert <- c(2, 4, 6:10, 14)
  data[, convert] <- data.frame(apply(data[convert], 2, as.factor))
  
  set.seed(123)
  # Boruta分析
  boruta.train <- Boruta(income~., data = data, doTrace = 2)
  
  print(boruta.train)
  
  # 產生圖表
  plot(boruta.train, xlab = "", xaxt = "n")
  
  lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
    boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
  
  names(lz) <- colnames(boruta.train$ImpHistory)
  Labels <- sort(sapply(lz,median))
  axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
  
  # 輸出分析數據
  getSelectedAttributes(boruta.train, withTentative = F)
  boruta.df <- attStats(boruta.train)
  print(boruta.df)
}