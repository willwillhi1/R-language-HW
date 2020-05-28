class_people <- function()
{
  data <- read.csv("D:/R語言-課程/anslist.csv")
  cat(sprintf("班級:    A    B    C    D    E    F    G    H   總數\n"))
  cat("____________________________________________________\n")
  cat(sprintf("人數:   %d   %d   %d   %d   %d   %d   %d   %d   %d\n",nrow(data[data$Class == "A",]), nrow(data[data$Class == "B",]), nrow(data[data$Class == "C",]), 
          nrow(data[data$Class == "D",]), nrow(data[data$Class == "E",]), nrow(data[data$Class == "F",]), nrow(data[data$Class == "G",]), nrow(data[data$Class == "H",]), nrow(data)))
  cat("\n")
}

score_people <- function()
{
  data <- read.csv("D:/R語言-課程/anslist.csv")
  check <- cbind(data$E1 == "B",data$E2 == "A",data$E3 == "A",data$E4 == "D",
                 data$E5 == "C",data$M1 == "20",data$M2 == "20",data$M3 == "30",
                 data$Q1 == "Cherry",data$Q2 == "Pear")
  check <- 1*check
  mul <- c(8, 8, 8, 8, 8, 10, 10, 10, 15, 15)
  check <- sweep(check, 2, mul, "*")
  check <- rowSums(check)
  cat(sprintf("分數:    0~10    10~20    20~30    30~40    40~50    50~60    60~70    70~80    80~90    90~100\n"))
  cat("_______________________________________________________________________________________________\n")
  cat(sprintf("人數:      %d      %d      %d       %d      %d       %d        %d        %d        %d         %d",sum(s>=0 & s<=10), sum(s>10 & s<=20), sum(s>20 & s<=30),
              sum(s>30 & s<=40), sum(s>40 & s<=50), sum(s>50 & s<=60), sum(s>60 & s<=70), sum(s>70 & s<=80), sum(s>80 & s<=90), sum(s>90 & s<=100)))
  cat("\n")
  cat("\n")
}

rank_allclass <- function()
{
  data <- read.csv("D:/R語言-課程/anslist.csv")
  data <- data[order(data[,2]),]
  check <- cbind(data$E1 == "B",data$E2 == "A",data$E3 == "A",data$E4 == "D",
                 data$E5 == "C",data$M1 == "20",data$M2 == "20",data$M3 == "30",
                 data$Q1 == "Cherry",data$Q2 == "Pear")
  check <- 1*check
  mul <- c(8, 8, 8, 8, 8, 10, 10, 10, 15, 15)
  check <- sweep(check, 2, mul, "*")
  check <- rowSums(check)
  rankall <- rank(-check, ties.method = "min")
  result <- as.vector(data_order$Sid[order(check,decreasing = TRUE)][1:sum(rankall<=10)])
  cat("全部前10名: ")
  cat(result)
  cat("\n")
  cat("\n")
}

rank_class <- function(x)
{
  #data[data$Class=="A",]
  #data <- read.csv("D:/R語言-課程/anslist.csv")
  data <- x[order(x[,2]),]
  check <- cbind(data$E1 == "B",data$E2 == "A",data$E3 == "A",data$E4 == "D",
                 data$E5 == "C",data$M1 == "20",data$M2 == "20",data$M3 == "30",
                 data$Q1 == "Cherry",data$Q2 == "Pear")
  check <- 1*check
  mul <- c(8, 8, 8, 8, 8, 10, 10, 10, 15, 15)
  check <- sweep(check, 2, mul, "*")
  check <- rowSums(check)
  rankall <- rank(-check, ties.method = "min")
  result <- as.vector(data_order$Sid[order(check,decreasing = TRUE)][1:sum(rankall<=5)])
  cat(as.character(data[1,1]))
  cat(" 班前 5 名: ")
  cat(result)
  cat("\n")
}

class_mean <- function()
{
  data <- read.csv("D:/R語言-課程/anslist.csv")
  #data <- data[order(data[,2]),]
  check <- cbind(data$E1 == "B",data$E2 == "A",data$E3 == "A",data$E4 == "D",
                 data$E5 == "C",data$M1 == "20",data$M2 == "20",data$M3 == "30",
                 data$Q1 == "Cherry",data$Q2 == "Pear")
  check <- 1*check
  mul <- c(8, 8, 8, 8, 8, 10, 10, 10, 15, 15)
  check <- sweep(check, 2, mul, "*")
  check <- data.frame(rowSums(check))
  #check <- data.frame(Class = as.character(data[,1]),score = check)
  cat(sprintf("班級:    A    B    C    D    E    F    G    H   全部\n"))
  cat("____________________________________________________\n")
  cat(sprintf("分數: %.1f %.1f %.1f %.1f %.1f %.1f %.1f %.1f  %.1f", round(mean(check[data$Class == "A",]),1), round(mean(check[data$Class == "B",]),1), round(mean(check[data$Class == "C",]),1),
              round(mean(check[data$Class == "D",]),1), round(mean(check[data$Class == "E",]),1), round(mean(check[data$Class == "F",]),1), round(mean(check[data$Class == "G",]),1), round(mean(check[data$Class == "H",]),1), 
              round(colMeans(check), 1)))
  cat("\n")
  cat("\n")
}


find_PH_PL <- function()
{
  data <- read.csv("D:/R語言-課程/anslist.csv")
  check <- cbind(data$E1 == "B",data$E2 == "A",data$E3 == "A",data$E4 == "D",
                 data$E5 == "C",data$M1 == "20",data$M2 == "20",data$M3 == "30",
                 data$Q1 == "Cherry",data$Q2 == "Pear")
  check <- 1*check
  mul <- c(8, 8, 8, 8, 8, 10, 10, 10, 15, 15)
  check <- sweep(check, 2, mul, "*")
  check <- data.frame(check = rowSums(check))
  distribute <- hist(check$check, breaks = seq(0, 100, 1))
  
  test <- cumsum(distribute$counts)
  t <- which(test <= round(474/4, 1))
  PL <- test[length(t)]    
  test <- distribute$counts
  test <- test[length(test):1]
  test <- cumsum(test)
  t <- which(test <= round(474/4, 1))
  PH <- test[length(t)+1]
  
  cat("高分組人數: ")
  cat(PH)
  cat(" 人   ")
  cat("低分組人數: ")
  cat(PL)
  cat(" 人   \n")
  cat("\n")
}

myfun <- function()
{
  data <- read.csv("D:/R語言-課程/anslist.csv")
  class_people()
  score_people()
  rank_allclass()
  rank_class(data[data$Class=="A",])
  rank_class(data[data$Class=="B",])
  rank_class(data[data$Class=="C",])
  rank_class(data[data$Class=="D",])
  rank_class(data[data$Class=="E",])
  rank_class(data[data$Class=="F",])
  rank_class(data[data$Class=="G",])
  rank_class(data[data$Class=="H",])
  cat("\n")
  class_mean()
  find_PH_PL()
  
}
