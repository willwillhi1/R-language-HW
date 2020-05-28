#UTF-8
scan_ans <- function()
{
  x <- scan("", what = integer(),n = 1, quiet = TRUE);
  return (x);
}

#刪除重複資料
init_data <- function(x)
{
  n1 <- floor(x/1000); 
  n2 <- (floor(x/100))%%10;
  n3 <- (floor(x/10))%%10;
  n4 <- x%%10;
  if(((n1 != n2) && (n1 != n3) && (n1 != n4)) && ((n2 != n3) && (n2 != n4)) && (n3 != n4))
  {
    return(c(x));
  }
  else
  {
    return(c(0)); 
  }
}

#篩選資料庫
select_database <- function(x, guess_ans, A, B)
{
  A_result <- 0;
  B_result <- 0;

  x_n1 <- floor(x/1000); 
  x_n2 <- (floor(x/100))%%10;
  x_n3 <- (floor(x/10))%%10;
  x_n4 <- x%%10;
  
  guess_n1 <- floor(guess_ans/1000); 
  guess_n2 <- (floor(guess_ans/100))%%10;
  guess_n3 <- (floor(guess_ans/10))%%10;
  guess_n4 <- guess_ans%%10;
  
  if(x_n1 == guess_n1) A_result <- A_result + 1;
  if(x_n2 == guess_n2) A_result <- A_result + 1;
  if(x_n3 == guess_n3) A_result <- A_result + 1;
  if(x_n4 == guess_n4) A_result <- A_result + 1;
  if(x_n1 == guess_n2 || x_n1 == guess_n3 || x_n1 == guess_n4) B_result <- B_result + 1;
  if(x_n2 == guess_n1 || x_n2 == guess_n3 || x_n2 == guess_n4) B_result <- B_result + 1;
  if(x_n3 == guess_n1 || x_n3 == guess_n2 || x_n3 == guess_n4) B_result <- B_result + 1;
  if(x_n4 == guess_n1 || x_n4 == guess_n2 || x_n4 == guess_n3) B_result <- B_result + 1;
  
  if(A == A_result && B == B_result)
  {
    return(x);  
  }
  else
  {
    return(0)    
  }
}

#電腦猜測答案
guess <- function(x)
{
  len <- length(x);
  index <- sample(1:len, 1);
  return(index);
}

#檢查AB格式
check_AB <- function(x)
{
  if(x == "0A0B" || x == "0A1B" || x == "0A2B" || x == "0A3B" || x == "0A4B" || x == "1A0B"
     || x == "1A1B" || x == "1A2B" || x == "1A3B" || x == "2A0B" || x == "2A1B" || x == "2A2B"
     || x == "3A0B" || x == "4A0B")
  {
    return(TRUE);
  }
  else if(nchar(x) != 4)
  {
    cat("長度不對!");
    return(FALSE);
  }
  else
  {
    if(grepl("[0-9]A[0-9]B", x) == 1)
    {
      cat("數字不合理!");
    }
    else
    {
      cat("格式錯誤");
    }
    return(FALSE);
  }
}

#主程式
main_func <- function()
{
  #x <- scan_ans();
  index <- 0;
  end <- 0;
  count <- 1;
  error <- 0;
  database <- aaply(matrix(1000:9999),  1, init_data);
  while(!end)
  {
    if(!error)
    {
      new_database <- which(database == 0);
      database <- database[-new_database];
      #cat(database); 方便DEBUG
      if(length(database) == 0)
      {
        cat("騙我!");
        break;
      }
      cat("猜第");
      cat(count);
      cat("次: ");
      index <- guess(database);
      cat(database[index]);
    }
    cat("\n請問幾A幾B (?A?B 格式輸入):\n")
    AB_ans <- scan("", what='character',n = 1, quiet = TRUE);
    if(!check_AB(AB_ans)) 
    {
      error <- 1;
      next;   
    }
    error <- 0;
    A <- as.integer(substr(AB_ans, 1 ,1));
    B <- as.integer(substr(AB_ans, 3 ,3));
    if(A == 4 && B == 0) end <- 1;
    database <- aaply(database,  1, select_database, database[index], A, B);
    count <- count + 1;
  }
}
