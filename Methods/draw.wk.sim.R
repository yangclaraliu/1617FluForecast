draw.wk.sim <- draw.peak.sim <- function(x){
  wk_bin <- data.frame(cbind(start=seq(0,13,0.1),end=c(seq(0.1,13,0.1),100)))
  wk_bin["perc"] <- NA
  for(i in 1:131){wk_bin$perc[i] <- length(which(x>=wk_bin$start[i] & x<=wk_bin$end[i]))/length(x) }
  return(wk_bin)
}