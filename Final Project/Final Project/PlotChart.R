plotChart <- function()
{
  dataset <- read.csv("~/R_FinalProject/Dataset/adult_data.csv", header = FALSE)
  
  colnames(dataset) <- c("age","workclass","fnlwgt","education","education.num"
                         ,"marital.status","occupation","relationship","race","sex","capital.gain",
                         "capital.loss","hours.per.week","native.country","pay")
  
  # 年齡
  ggplot(dataset, aes(age, fill = factor(pay))) + geom_histogram(stat = "count",position = "fill")
  # 受教育時長
  ggplot(dataset, aes(education.num, fill = factor(pay))) + geom_histogram(stat = "count",position = "fill")
  # 每週工時
  ggplot(dataset, aes(dataset$`hours.per.week`, fill = pay)) + geom_histogram(stat = "count", position = "fill")
  # 種族
  ggplot(dataset, aes(race, fill = pay)) + geom_histogram(stat = "count", position = "fill")
  # 資本收益
  ggplot(dataset, aes(dataset$`capital.gain`, fill = factor(pay))) + geom_histogram(position = "fill")
  # 資本損失
  ggplot(dataset, aes(capital.loss, fill = pay)) + geom_histogram(position = "fill", bins = 1000)
}