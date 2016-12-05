require(cdcfluview); require(scales); require(forecast); require(cardidates)
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast")
 submission <- read.csv("Long_Flu_Submission_Template_update.csv")
 submission$Value <- NA
 baseline <- read.csv("wILI_Baseline.csv",stringsAsFactors = F)
 epidemic_week <- c(40:52,1:20)
 submission_date <- seq(as.Date("2016-11-07"),as.Date("2017-05-15"),7)
 report = 5
 sapply(c("draw.onsettime.sim.R","draw.peak.sim.R","draw.peakweek.sim.R","draw.wk.sim.R"),source)
 
nsim = 1000
for(h in 1:10){
  flu <- get_flu_data("hhs",h,"ilinet",c(2002:2016))
  #plot(flu$`% WEIGHTED ILI`,type='l')
  start <- which(flu$WEEK==40)
  end <- c(which(flu$WEEK==39),nrow(flu))
  flu["season"]<-NA;for(i in 1:length(start)){flu$season[start[i]:end[i]]<-i}
  #plot(flu$season)
  flu$`% WEIGHTED ILI` <- ts(as.numeric(flu$`% WEIGHTED ILI`),freq=52)
  #acf(flu$`% WEIGHTED ILI`)
  #pacf(flu$`% WEIGHTED ILI`)
  #nonstationary behavior
  #tsdisplay(flu$`% WEIGHTED ILI`)
  #tsdisplay(diff(flu$`% WEIGHTED ILI`))
  flu$`% WEIGHTED ILI`[flu$`% WEIGHTED ILI`==0] <- NA
  model <- Arima(log(flu$`% WEIGHTED ILI`),c(3,1,0),c(0,1,2))
  #tsdisplay(model$residuals)
  week_in <- tail(flu$WEEK,1)-40+1
  sim_res <- data.frame(matrix(NA,nrow=52-week_in,ncol=nsim))
  peaksize <- peaktime <- onsettime <- rep(NA,nsim)
  for(i in 1:nsim){
    repeat{
      simulated_data <- exp(simulate(model,nsim=52-week_in,future=T))
      #check 2 things. (1)
      toobig <- any(simulated_data > 10)
      #check 2 things. (2)
      peaktime_temp <- which.max(unlist(c(flu[flu$season==15,"% WEIGHTED ILI"],simulated_data)))
#       peaktime_index <- peakwindow(simulated_data)$smd.max.index
#       if(peaktime_temp>52) peaktime_temp <- peaktime_temp-52
      toolate <- ( peaktime_temp > 26)
      if(toobig == FALSE & toolate == FALSE) break
    }
    #plot(c(exp(model$x),simulated_data),type='l')
    #abline(h=baseline[1,12],col="red",lty=2)
    #abline(v=nrow(flu),col="blue",lty=2)
    sim_res[,i] <- simulated_data
    peaksize[i] <- max(unlist(c(flu[flu$season==15,"% WEIGHTED ILI"],simulated_data)))
    onsettime_temp <- (intersect(intersect(which(c(tail(exp(model$x),week_in),simulated_data)>baseline[h+1,12]),which(c(tail(exp(model$x),week_in),simulated_data)>baseline[h+1,12])+1),which(c(tail(exp(model$x),week_in),simulated_data)>baseline[h+1,12])+2)-2)
    if (length(onsettime_temp)!=0)  {onsettime[i] <- min(onsettime_temp)} else {onsettime[i] <- NA}
    peaktime[i] <- peaktime_temp
  }
onsettime_point <- ((intersect(intersect(which(c(tail(exp(model$x),week_in),exp(forecast(model,52-week_in)$mean))>baseline[h+1,12]),which(c(tail(exp(model$x),week_in),exp(forecast(model,52-week_in)$mean))>baseline[h+1,12])+1),which(c(tail(exp(model$x),week_in),exp(forecast(model,52-week_in)$mean))>baseline[h+1,12])+2)-2))
  if(length(onsettime_point)!=0) {
    onsettime_point <- min(onsettime_point)
    onsettime_point <- epidemic_week[onsettime_point]
    } else {onsettime_point <- NA}
  point_res <- c(onsettime_point,epidemic_week[which.max(c(tail(exp(model$x),week_in),exp(forecast(model,52-week_in)$mean)))],max(c(tail(exp(model$x),week_in),exp(forecast(model,52-week_in)$mean))))
  #point_res
  res <- c(epidemic_week[which.max(draw.onset.sim(onsettime)[1:33,2])],
           draw.onset.sim(onsettime)[,2],
           point_res[2],
           draw.peakweek.sim(peaktime)[,2],
           point_res[3],
           draw.peak.sim(peaksize)[,3],
           exp(forecast(model,52-week_in)$mean[1]),
           draw.wk.sim(sim_res[1,])[,3],
           exp(forecast(model,52-week_in)$mean[2]),
           draw.wk.sim(sim_res[2,])[,3],
           exp(forecast(model,52-week_in)$mean[3]),
           draw.wk.sim(sim_res[3,])[,3],
           exp(forecast(model,52-week_in)$mean[4]),
           draw.wk.sim(sim_res[4,])[,3])
  setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/SARIMA/RawResults/Report5")
  save(point_res,sim_res,file=paste("HHS",h,".RData",sep=""))
  
##############Save the results by HHS first###########################
  setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/SARIMA/Weekly_Forecasts_Plots/Report5")
  png.name <- paste(paste(paste("HHS",h,sep=""),submission_date[report],sep="-"),"-4wk.png",sep="")
  png(png.name)
  plot(1,xlab="Epidemic Week (Starting from Week 40 Calendar Year)",ylab="ILI%",ylim=range(unlist(sim_res[1:4,])),type="n",xlim=c(1,week_in+4),main=paste("EW",tail(flu$WEEK,1),sep=""))
  points(x=jitter(rep(week_in+1,nsim)),y=sim_res[1,],col=alpha("grey",0.5),pch=20)
  points(x=jitter(rep(week_in+2,nsim)),y=sim_res[2,],col=alpha("grey",0.5),pch=20)
  points(x=jitter(rep(week_in+3,nsim)),y=sim_res[3,],col=alpha("grey",0.5),pch=20)
  points(x=jitter(rep(week_in+4,nsim)),y=sim_res[4,],col=alpha("grey",0.5),pch=20)
  points(tail(flu$`% WEIGHTED ILI`,week_in),col="green",pch=20)
  points(y=exp(forecast(model,52-week_in)$mean[1:4]),x=c((week_in+1):(week_in+4)),pch=20,col="red")
  lines(tail(flu$`% WEIGHTED ILI`,week_in),col="green",lwd=2)
  lines(y=c(tail(flu$`% WEIGHTED ILI`,1),exp(forecast(model,52-week_in)$mean[1:4])),x=c((week_in):(week_in+4)),col="red",lty=2,lwd=2)
  abline(h=baseline[h+1,12], lty=2)
  dev.off()
  name <- paste("HHS Region",h)
  submission[submission$Location==name,"Value"] <- res
  setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/SARIMA/Submission")
  file_name <- paste(paste(paste("EW",tail(flu$WEEK,1),sep=""),"HumNat2",submission_date[report],sep="-"),".csv",sep="")
  write.csv(submission,file_name)
}


###########################National Level#####################################
flu <- get_flu_data("national",data_source="ilinet",years=c(2002:2016))
plot(flu$`% WEIGHTED ILI`,type='l')
start <- which(flu$WEEK==40)
end <- c(which(flu$WEEK==39),nrow(flu))
flu["season"]<-NA;for(i in 1:length(start)){flu$season[start[i]:end[i]]<-i}
#plot(flu$season)
flu$`% WEIGHTED ILI` <- ts(as.numeric(flu$`% WEIGHTED ILI`),freq=52)
# acf(flu$`% WEIGHTED ILI`)
# pacf(flu$`% WEIGHTED ILI`)
#nonstationary behavior
# tsdisplay(flu$`% WEIGHTED ILI`)
# tsdisplay(diff(flu$`% WEIGHTED ILI`))
flu$`% WEIGHTED ILI`[flu$`% WEIGHTED ILI`==0] <- NA
model <- Arima(log(flu$`% WEIGHTED ILI`),c(3,1,0),c(0,1,2))
# tsdisplay(model$residuals)
week_in <- tail(flu$WEEK,1)-40+1
sim_res <- data.frame(matrix(NA,nrow=52-week_in,ncol=nsim))
peaksize <- peaktime <- onsettime <- rep(NA,nsim)
for(i in 1:nsim){
  repeat{
    simulated_data <- exp(simulate(model,nsim=52-week_in,future=T))
    #check 2 things. (1)
    toobig <- any(simulated_data > 10)
    #check 2 things. (2)
    peaktime_temp <- unlist(c(flu[flu$season==15,"% WEIGHTED ILI"],simulated_data))
    #       peaktime_index <- peakwindow(simulated_data)$smd.max.index
    #       if(peaktime_temp>52) peaktime_temp <- peaktime_temp-52
    toolate <- ( peaktime_temp > 26)
    if(toobig == FALSE & toolate == FALSE) break
  }
  #plot(c(exp(model$x),simulated_data),type='l')
  #abline(h=baseline[1,12],col="red",lty=2)
  #abline(v=nrow(flu),col="blue",lty=2)
  sim_res[,i] <- simulated_data
  peaksize[i] <- max(unlist(c(flu[flu$season==15,"% WEIGHTED ILI"],simulated_data)))
  onsettime_temp <- (intersect(intersect(which(c(tail(exp(model$x),week_in),simulated_data)>baseline[1,12]),which(c(tail(exp(model$x),week_in),simulated_data)>baseline[1,12])+1),which(c(tail(exp(model$x),week_in),simulated_data)>baseline[1,12])+2)-2)
  if (length(onsettime_temp)!=0)  {onsettime[i] <- min(onsettime_temp)} else {onsettime[i] <- NA}
  peaktime[i] <- peaktime_temp
}
onsettime_point <- ((intersect(intersect(which(c(tail(exp(model$x),week_in),exp(forecast(model,52-week_in)$mean))>baseline[1,12]),which(c(tail(exp(model$x),week_in),exp(forecast(model,52-week_in)$mean))>baseline[1,12])+1),which(c(tail(exp(model$x),week_in),exp(forecast(model,52-week_in)$mean))>baseline[1,12])+2)-2))
if(length(onsettime_point)!=0) {
  onsettime_point <- min(onsettime_point)
  onsettime_point <- epidemic_week[onsettime_point]
} else {onsettime_point <- NA}
point_res <- c(onsettime_point,epidemic_week[which.max(c(tail(exp(model$x),week_in),exp(forecast(model,52-week_in)$mean)))],max(c(tail(exp(model$x),week_in),exp(forecast(model,52-week_in)$mean))))
#point_res
res <- c(epidemic_week[which.max(draw.onset.sim(onsettime)[1:33,2])],
         draw.onset.sim(onsettime)[,2],
         point_res[2],
         draw.peakweek.sim(peaktime)[,2],
         point_res[3],
         draw.peak.sim(peaksize)[,3],
         exp(forecast(model,52-week_in)$mean[1]),
         draw.wk.sim(sim_res[1,])[,3],
         exp(forecast(model,52-week_in)$mean[2]),
         draw.wk.sim(sim_res[2,])[,3],
         exp(forecast(model,52-week_in)$mean[3]),
         draw.wk.sim(sim_res[3,])[,3],
         exp(forecast(model,52-week_in)$mean[4]),
         draw.wk.sim(sim_res[4,])[,3])
#
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/SARIMA/RawResults/Report5")
save(point_res,sim_res,file=paste("National",".RData",sep=""))

##############Save the results at National Level###########################
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/SARIMA/Weekly_Forecasts_Plots/Report5")
png.name <- paste(paste("National",submission_date[report],sep="-"),"-4wk.png",sep="")
png(png.name)
plot(1,xlab="Epidemic Week (Starting from Week 40 Calendar Year)",ylab="ILI%",ylim=range(unlist(sim_res[1:4,])),type="n",xlim=c(1,week_in+4),main=paste("EW",tail(flu$WEEK,1),sep=""))
points(x=jitter(rep(week_in+1,nsim)),y=sim_res[1,],col=alpha("grey",0.5),pch=20)
points(x=jitter(rep(week_in+2,nsim)),y=sim_res[2,],col=alpha("grey",0.5),pch=20)
points(x=jitter(rep(week_in+3,nsim)),y=sim_res[3,],col=alpha("grey",0.5),pch=20)
points(x=jitter(rep(week_in+4,nsim)),y=sim_res[4,],col=alpha("grey",0.5),pch=20)
points(tail(flu$`% WEIGHTED ILI`,week_in),col="green",pch=20)
points(y=exp(forecast(model,52-week_in)$mean[1:4]),x=c((week_in+1):(week_in+4)),pch=20,col="red")
lines(tail(flu$`% WEIGHTED ILI`,week_in),col="green",lwd=2)
lines(y=c(tail(flu$`% WEIGHTED ILI`,1),exp(forecast(model,52-week_in)$mean[1:4])),x=c((week_in):(week_in+4)),col="red",lty=2,lwd=2)
abline(h=baseline[1,12], lty=2)
dev.off()
name <- "US National"
submission[submission$Location==name,"Value"] <- res
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/SARIMA/Submission")
file_name <- paste(paste(paste("EW",tail(flu$WEEK,1),sep=""),"HumNat2",submission_date[report],sep="-"),".csv",sep="")
write.csv(submission,file_name,row.names = F)


