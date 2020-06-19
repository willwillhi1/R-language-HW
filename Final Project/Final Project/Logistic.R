logistic <- function()
{
  dataset <- read.csv("~/R_FinalProject/Dataset/adult_data2.csv", header = FALSE)
  
  colnames(dataset) <- c("age","workclass","fnlwgt","education","education.num"
                         ,"marital.status","occupation","relationship","race","sex","capital.gain",
                         "capital.loss","hours.per.week","native.country","pay")
  dataset <- dataset[,c(1,5,11,12,15)]
  model <- glm(pay ~ age + education.num + capital.gain + capital.loss, data = dataset)
  
  
  print(summary(model))
  print(exp(6.352e-03))
  print(exp(4.930e-02))
  print(exp(1.013e-05))
  print(exp(1.280e-04))
}