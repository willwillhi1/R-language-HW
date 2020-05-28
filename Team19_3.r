myfunc <- function()
{
  ans <- sample(49, 6);
  cat("開獎號碼: ")
  cat(ans)
  cat("\n\n請輸入投注號碼，以空格分隔，輸入6個0可結束\n")
  count <- 0
  while(1)
  {
    count <- count + 1
    cat("第 ")
    cat(count)
    cat(" 組投注號碼:\n")
    x <- scan("", n = 6, sep = " ", quiet = TRUE)
    test <- TRUE
    if(sum(x) == 0) break
    #錯誤輸入判斷
    else
    {
      for(i in 1:6)
        if(x[i] > 49 || x[i] < 1)
          test <- FALSE
      for(i in 1:6)
        for(j in 1:6)
        {
          if(i == j) next
          else if(x[i] == x[j])
          {
            test <- FALSE
            break
          }
        }
    }
    if(!test) 
    {
      cat("輸入錯誤!\n\n")
      next
    }
    win_num <- c()
    for(i in 1:6)
    {
      for(j in 1:6)
      {
        if(ans[i] == x[j])
          win_num <- append(win_num, ans[i])
      }
    }
    if(length(win_num) > 2)
    {
      cat("對中 ")
      cat(length(win_num))
      cat(" 個號碼: ")
      cat(win_num)
      cat(" 恭喜中獎\n\n")
    }
    else
    {
      cat("對中 ")
      cat(length(win_num))
      cat(" 個號碼: ")
      cat(win_num)
      cat(" 銘謝惠顧\n\n")
    }
  }
}