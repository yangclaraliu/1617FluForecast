draw.peak.sim <- function(x){
  peak_bin <- data.frame(cbind(start=seq(0,13,0.1),end=c(seq(0.1,13,0.1),100)))
  peak_bin["perc"] <- NA
  for(i in 1:131){peak_bin$perc[i] <- length(which(x>=peak_bin$start[i] & x<=peak_bin$end[i]))/length(x) }
  return(peak_bin)
}


