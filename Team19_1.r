myfunc <- function(num)
{
  range <- 2*num-1
  
  for(i in 1:num)
  {
    index <- (range - (2*i-1))/2
    for(j in 1:range)
    {
      if(j > index && j <= (index + (2*i-1)))
      {
        ch <- i%%26
        if(ch == 0)
            ch <- 26
        cat(LETTERS[ch], sep = "")
      }
      else if(j <= index)
      {
        cat(" ", sep = "")
      }
    }
    cat("\n")
  }
}