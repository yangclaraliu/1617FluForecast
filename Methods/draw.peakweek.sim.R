draw.peakweek.sim <- function(x){
  peakweek_bin <- as.data.frame(c(40:52,1:20));colnames(peakweek_bin) <- "woy"
  peakweek_bin["perc"] <- NA
  for(i in 1:33){ peakweek_bin$perc[i] <- length(which(x==peakweek_bin$woy[i]))/length(x) }
  return(peakweek_bin)
}
