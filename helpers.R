safe_div <- function(a, b){
  ifelse(is.na(b) | b == 0, 0, a / b)
}

zscore <- function(x){
  if(sd(x, na.rm=TRUE) == 0){
    return(rep(0, length(x)))
  }
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}