lags <- function(x,lags){
  res <- c(rep(NA,lags),head(x,(length(x)-lags)))
  return(res)
}