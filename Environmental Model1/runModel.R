load("~/Google Drive/Influenza/16-17_forecast/Environmental_Regression/model_HHS.Rdata")
#load("~/Google Drive/Influenza/16-17_forecast/Environmental_Regression/fore_tab_all.RData")
load("~/Google Drive/Influenza/16-17_forecast/Environmental_Regression/fore_tab_all/fore_tab_all_EW43_EN39.RData")
require(scales)
#load("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression/model_seasonality.RData")
#load("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression/fore_tab.RData")
setwd("~/Google Drive/Influenza/16-17_forecast")
baseline <- read.csv("wILI_Baseline.csv",stringsAsFactors = F)
submission_date <- seq(as.Date("2016-11-07"),as.Date("2017-05-15"),7)
report = 1
sim_res_all <- list()

for(h in 1:10){
  to.fill <- which(is.na(fore_tab_all[[h]]$ILIp))
  new_tab <- fore_tab_all[[h]][to.fill,]
  new_tab_blank <- new_tab
  
  simulate.model <- function(i){
    new_tab <- new_tab_blank
    repeat{
      pred <- predict(model_HHS[[h]],newdata=new_tab,se=T) 
      pred_mean <- pred$fit[!is.na(pred$fit)]
      pred_mean <- pred_mean[length(pred_mean)]
      pred_se <- pred$se.fit[!is.na(pred$se.fit)]
      forecast.togo <- length(which(is.na(new_tab$ILIp)))
      pred_se <- pred_se[length(pred_se)]*sqrt((forecast.togo))
      new_val <- rnorm(1,mean=pred_mean,sd=pred_se)
      for (col in c(4,6:10)){ new_tab[is.na(new_tab[,col]),col][1:length(new_val)] <- new_val}
      if (all(!is.na(new_tab$ILIp))) {break}
    }
    return((exp(new_tab$ILIp)))
    rm(new_tab,pred,pred_mean,pred_se,forecast.togo,new_val)
  }
  
  nsim=1000
  sim_res <- sapply(c(1:nsim),simulate.model)
  sim_res <- sim_res[,!(apply(sim_res,2,which.max)>26)]
  sim_res <- sim_res[,!apply(sim_res,2, function(x) any(x>10))]
  sim_res <- as.data.frame(sim_res)
  
  peaksize <- peaktime <- onsettime <- rep(NA,ncol(sim_res))
  epi_weeks <- c(40:52,1:39)
  for(i in 1:ncol(sim_res)){
    ILIp <- c(fore_tab_all[[h]]$ILIp,sim_res[,i])
    ILIp <- ILIp[!is.na(ILIp)]
    ILIp_15 <- unlist(tail(ILIp,52))
    onsettime_temp <- (intersect(intersect(which(ILIp_15>baseline[h+1,12]),which(ILIp_15>baseline[h+1,12])+1),which(ILIp_15>baseline[h+1,12])+2)-2)
    if (length(onsettime_temp)!=0)  {onsettime[i] <- epi_weeks[min(onsettime_temp)]} else {onsettime[i] <- NA}
    peaksize[i] <- max(ILIp_15)
    peaktime[i] <- epi_weeks[which.max(ILIp_15)]
  }
  
  # rm(new_tab)
  # par(mfrow=c(1,2))
  # plot(ILIp)
  # plot((exp(new_tab$ILIp)))
  # epi_weeks <- c(40:52,1:39)
  # epi_weeks[which.max(tail(ILIp,52))]
  # #abline(v=52,lty=2)
  
  week_in <- 52-nrow(new_tab)
  #setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression/Weekly_Forecasts_Plots")
  setwd("~/Google Drive/Influenza/16-17_forecast/Environmental_Regression/Weekly_Forecasts_Plots/Report1")
  png.name <- paste(paste(paste("HHS",h,sep=""),submission_date[1],sep="-"),"-4wk.png",sep="")
  png(png.name)
  plot(1,xlab="Epidemic Week (Starting from Week 40 Calendar Year)",ylab="ILI%",ylim=range(unlist(sim_res[1:4,])),type="n",xlim=c(1,week_in+4),main=paste("EW",40+week_in-1,sep=""))
  points(x=jitter(rep(week_in+1,ncol(sim_res))),y=sim_res[1,],col=alpha("grey",0.5),pch=20)
  points(x=jitter(rep(week_in+2,ncol(sim_res))),y=sim_res[2,],col=alpha("grey",0.5),pch=20)
  points(x=jitter(rep(week_in+3,ncol(sim_res))),y=sim_res[3,],col=alpha("grey",0.5),pch=20)
  points(x=jitter(rep(week_in+4,ncol(sim_res))),y=sim_res[4,],col=alpha("grey",0.5),pch=20)
  points(head(ILIp_15,week_in),col="green",pch=20)
  points(y=rowMeans(sim_res[1:4,]),x=c((week_in+1):(week_in+4)),pch=20,col="red")
  lines(head(ILIp_15,week_in),col="green",lwd=2)
  lines(y=c(tail(head(ILIp_15,week_in),1),rowMeans(sim_res[1:4,])),x=c((week_in):(week_in+4)),col="red",lty=2,lwd=2)
  abline(h=baseline[h+1,12], lty=2)
  dev.off()
  
  sim_res_all[[h]] <- sim_res
}
name <- paste("ENVR_RawResult_",submission_date[report],".RData",sep="")
name
setwd("~/Google Drive/Influenza/16-17_forecast/Environmental_Regression/RawResults")
save(sim_res_all,file=name)


