plotACC_Time <- function()
{
  dataset <- read.csv("~/R_FinalProject/Dataset/Result.csv", header = FALSE)
  
  colnames(dataset) <- c('Type', 'Features.num', 'Time', 'Acc')
  
  # 特徵數與訓練時間關係
  ggplot(dataset, aes(Features.num, Time, colour = Type)) + geom_point() + geom_line()
  # 特徵數與準確度關係
  ggplot(dataset, aes(Features.num, Acc, colour = Type)) + geom_point() + geom_line()
}