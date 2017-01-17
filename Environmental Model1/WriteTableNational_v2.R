require(cdcfluview); require(scales)
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression")
#Gai
load("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression/RawResults/ENVR_RawResult_2017-01-16.RData")
load("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression/local_to_national_model.RData")
#Gai
load("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression/fore_tab_all/fore_tab_all_EW1_EN48.RData")
source("draw.wk.sim.R");source("draw.onsettime.sim.R"); source("draw.peakweek.sim.R");source("draw.peakweek.sim.R")
baseline <- read.csv("wILI_Baseline.csv",stringsAsFactors = F)
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression/Submissions")
base <- get_flu_data("national",,"ilinet",c(2002:2016))
submission <- read.csv(list.files(pattern=paste("EW",tail(base$WEEK,1),sep="")),stringsAsFactors = F)
epi_weeks <- c(40:52,1:39)
submission_date <- seq(as.Date("2016-11-07"),as.Date("2017-05-15"),7)
#Gai
report = 11

sim_res_national <- data.frame(matrix(NA,ncol=1000,nrow=nrow(sim_res_all[[1]])))
for(i in 1:1000){
  sample1 <- function(h){sim_res_all[[h]][,sample(c(1:ncol(sim_res_all[[h]])),1)]}
  selected <- data.frame(sapply(c(1:10),sample1))
  colnames(selected) <- paste("HHS",c(1:10),sep="")
  sim_res_national[,i] <- (predict(local_to_national_model,selected)) 
}

peaksize <- peaktime <- onsettime <- rep(NA,ncol(sim_res_national))
for(i in 1:ncol(sim_res_national)){
  ILIp <- c(base$`% WEIGHTED ILI`,sim_res_national[,i])
  ILIp <- ILIp[!is.na(ILIp)]
  ILIp_15 <- unlist(tail(ILIp,52))
  onsettime_sim_res_national <- (intersect(intersect(which(ILIp_15>baseline[1,12]),which(ILIp_15>baseline[1,12])+1),which(ILIp_15>baseline[1,12])+2)-2)
  if (length(onsettime_sim_res_national)!=0)  {onsettime[i] <- min(onsettime_sim_res_national)} else {onsettime[i] <- NA}
  peaksize[i] <- max(ILIp_15)
  peaktime[i] <- which.max(ILIp_15)
}

res <- c(epi_weeks[round(mean(onsettime,na.rm=T))],draw.onset.sim(onsettime)[,2],
         epi_weeks[round(mean(peaktime))],draw.peakweek.sim(peaktime)[,2],
         mean(peaksize),draw.peak.sim(peaksize)[,3],
         mean(unlist(sim_res_national[1,])),draw.wk.sim((unlist(sim_res_national[1,])))[,3],
         mean(unlist(sim_res_national[2,])),draw.wk.sim((unlist(sim_res_national[2,])))[,3],
         mean(unlist(sim_res_national[3,])),draw.wk.sim((unlist(sim_res_national[3,])))[,3],
         mean(unlist(sim_res_national[4,])),draw.wk.sim((unlist(sim_res_national[4,])))[,3])
submission[submission$Location=="US National","Value"] <- res
file_name <- paste(paste(paste("EW",tail(base$WEEK,1),sep=""),"HumNat",submission_date[report],sep="-"),".csv",sep="")
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression/Submissions")
write.csv(submission,file_name,row.names = F)

#
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression/Weekly_Forecasts_Plots/Report11")

png.name <- paste(paste("National",submission_date[report],sep="-"),"-4wk.png",sep="")
png(png.name)
week_in <- tail(base$WEEK,1)-40+1+52
plot(1,xlab="Epidemic Week (Starting from Week 40 Calendar Year)",ylab="ILI%",ylim=c(1,5),type="n",xlim=c(1,week_in+4),main=paste("EW",40+week_in-1,sep=""))
points(x=jitter(rep(week_in+1,ncol(sim_res_national))),y=sim_res_national[1,],col=alpha("grey",0.5),pch=20)
points(x=jitter(rep(week_in+2,ncol(sim_res_national))),y=sim_res_national[2,],col=alpha("grey",0.5),pch=20)
points(x=jitter(rep(week_in+3,ncol(sim_res_national))),y=sim_res_national[3,],col=alpha("grey",0.5),pch=20)
points(x=jitter(rep(week_in+4,ncol(sim_res_national))),y=sim_res_national[4,],col=alpha("grey",0.5),pch=20)
points(head(ILIp_15,week_in),col="green",pch=20)
points(y=rowMeans(sim_res_national[1:4,]),x=c((week_in+1):(week_in+4)),pch=20,col="red")
lines(head(ILIp_15,week_in),col="green",lwd=2)
lines(y=c(tail(head(ILIp_15,week_in),1),rowMeans(sim_res_national[1:4,])),x=c((week_in):(week_in+4)),col="red",lty=2,lwd=2)
abline(h=baseline[1,12], lty=2)
dev.off()