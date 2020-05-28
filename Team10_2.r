

random <- function(argu)
{
  x <- c("台北市","台中市","基隆市","台南市","高雄市","台北縣","宜蘭縣","桃園縣","新竹縣","苗栗縣","台中縣","南投縣","彰化縣","雲林縣","嘉義縣","台南縣","高雄縣","屏東縣","花蓮縣","台東縣","澎湖縣","陽明山","嘉義市","新竹市")
  y <- c("A","B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","X","Y","I","O")
  z <- c(10:31,34,35)
  index <- sample(c(1:24),1)
  city <- cbind("縣市別"=x,"英文代碼"=y,"數字編碼"=z)
  letter <- as.character(city[index,2])
  num1 <- as.character(sample(1:2,1))
  num2 <- as.character(sample(0:9,1))
  num3 <- as.character(sample(0:9,1))
  num4 <- as.character(sample(0:9,1))
  num5 <- as.character(sample(0:9,1))
  num6 <- as.character(sample(0:9,1))
  num7 <- as.character(sample(0:9,1))
  num8 <- as.character(sample(0:9,1))
  num9 <- as.character(sample(0:9,1))
  #cat(letter)
  #cat(num1)
  #cat(num2, sep="")
  id <- paste0(letter,num1,num2,num3,num4,num5,num6,num7,num8,num9)
  area <- as.character(city[index,1])
  area_value_ten <- as.integer(city[index,3])/10
  area_value_one <- as.integer(city[index,3])%%10
  Y_value <- area_value_ten*1 + area_value_one*9 + as.integer(num1)*8 + as.integer(num2)*7 + as.integer(num3)*6 + as.integer(num4)*5 + as.integer(num5)*4 + as.integer(num6)*3 + 
    as.integer(num7)*2 + as.integer(num8)*1 + as.integer(num9)*1
  correct <- ifelse(trunc(Y_value)%%10==0,"Y","N")
  return(data.frame("身分證ID" = id, "區域" = area, "Y值" = trunc(Y_value), "正確性" = correct))
}

myfun <- function(count)
{
  ID <- adply(matrix(1:count, ncol = 1), 1, random, .id = "No")
  ID <- ID[order(ID$Y值),]
  ID <- ID[order(substr(ID$身分證ID,1,1)),]
  N_ID <- ID[which(ID[,5] == "N"),]
  Y_ID <- ID[which(ID[,5] == "Y"),]
  #d_ply(N_ID, 2, show)
  #N_ID
  cat(" 身分證ID    區域   Y值  正確性\n")
  cat("____________________________\n")
  cat(" ")
  cat(sprintf("%s %s %03d    %s \n", Y_ID$身分證ID, Y_ID$區域, Y_ID$Y值, Y_ID$正確性))
  cat(" ")
  cat(sprintf("%s %s %03d    %s \n", N_ID$身分證ID, N_ID$區域, N_ID$Y值, N_ID$正確性))
  cat("____________________________\n")
  cat("ID總數: ")
  cat(count)
  cat("   ")
  cat("正確ID數: ")
  cat(nrow(Y_ID))
  cat("(")
  cat(round(nrow(Y_ID)/count*100, 0))
  cat("%)\n")
}

