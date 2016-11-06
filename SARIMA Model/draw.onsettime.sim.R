draw.onset.sim <- function(x){
  onset_bin <- as.data.frame(c(40:52,1:20,NA));colnames(onset_bin) <- "woy"
  onset_bin["perc"] <- NA
  for(i in 1:33){ onset_bin$perc[i] <- length(which(x==onset_bin$woy[i]))/length(x) }
  onset_bin$perc[34] <- length(which(is.na(x)))/length(x)
  return(onset_bin)
}
