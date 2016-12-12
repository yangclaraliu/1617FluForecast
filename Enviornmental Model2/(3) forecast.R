require(cardidates); require(scales)
load("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental+Functional/M2_models.RData")
load("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental+Functional/foretab/foretab_EW48.RData")
source("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental+Functional/lags.R")

#load("~/Google Drive/Influenza/16-17_forecast/Environmental+Functional/M2_models.RData")
#load("~/Google Drive/Influenza/16-17_forecast/Environmental+Functional/foretab/foretab_EW47.RData")
#source("~/Google Drive/Influenza/16-17_forecast/Environmental+Functional/lags.R")
submission_date <- seq(as.Date("2016-11-07"),as.Date("2017-05-15"),7)
report = 6

setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast")
source("draw.wk.sim.R");source("draw.onsettime.sim.R"); source("draw.peakweek.sim.R");source("draw.peakweek.sim.R")
baseline <- read.csv("wILI_Baseline.csv",stringsAsFactors = F)


week_in = max(which(!is.na(tail(foretab[[1]]$y,52))))
restab_temp <- restab <- list()
nsim = 1000
#layer 1
for(h in c(1,3,4)){
  for(sim in 1:nsim){
    restab_temp[[sim]] <- foretab[[h]]
    repeat{
      pred <- predict(model.FN[[h]],newdata=restab_temp[[sim]],se=T) 
      pred_mean <- pred$fit[!is.na(pred$fit)]
      pred_mean <- pred_mean[length(pred_mean)]
      pred_se <- pred$se.fit[!is.na(pred$se.fit)]
      forecast.togo <- length(which(is.na(restab_temp[[sim]]$y)))
      pred_se <- sample(pred_se,1)*sqrt(forecast.togo)
      new_val <- rnorm(1,mean=pred_mean,sd=pred_se)
      for (col in c(ncol(restab_temp[[sim]]),which(grepl("Ilag",colnames(restab_temp[[sim]]))))){ 
        restab_temp[[sim]][is.na(restab_temp[[sim]][,col]),col][1:length(new_val)] <- new_val}
      if (all(!is.na(restab_temp[[sim]]$y))) {break}
    }
    plot(tail(exp(restab_temp[[sim]]$y),52))
    points(y=tail(exp(restab_temp[[sim]]$y),52-week_in),x=tail(1:52,52-week_in),col="red",pch=20)
  }
  restab[[h]] <- restab_temp
}

#layer 2
for(h in c(2,5)){
  #long distance relationships
  target <- colnames(foretab[[h]])[which(grepl("Ilag.HHS",colnames(foretab[[h]])))]
  target <- data.frame(matrix(unlist(strsplit(gsub("Ilag.HHS","",target),"[.]")),ncol=2,byrow=T))
  colnames(target) <- c("Region","Lag")
  target["Location"] <- which(grepl("Ilag.HHS",colnames(foretab[[h]])))
  
  for(sim in 1:nsim){
    restab_temp[[sim]] <- foretab[[h]]
    for(var in 1:nrow(target)){
      restab_temp[[sim]][,target$Location[var]] <- lags(restab[[target$Region[var]]][[sim]]$y,as.numeric(as.character(target$Lag[var])))
    }
    
    print(any(is.na(tail(restab_temp[[sim]][,target$Location],52))))
    repeat{
      pred <- predict(model.FN[[h]],newdata=restab_temp[[sim]],se=T) 
      pred_mean <- pred$fit[!is.na(pred$fit)]
      pred_mean <- pred_mean[length(pred_mean)]
      pred_se <- pred$se.fit[!is.na(pred$se.fit)]
      forecast.togo <- length(which(is.na(restab_temp[[sim]]$y)))
      pred_se <- sample(pred_se,1)*sqrt(forecast.togo)#pred_se[length(pred_se)]##*multiplier[week_in]#
      new_val <- rnorm(1,mean=pred_mean,sd=pred_se)
      for (col in c(ncol(restab_temp[[sim]]),which(grepl("Ilag",colnames(restab_temp[[sim]]))))){ 
        restab_temp[[sim]][is.na(restab_temp[[sim]][,col]),col][1:length(new_val)] <- new_val}
      if (all(!is.na(restab_temp[[sim]]$y))) {break}
    }
    plot(tail(exp(restab_temp[[sim]]$y),52))
    points(y=tail(exp(restab_temp[[sim]]$y),52-week_in),x=tail(1:52,52-week_in),col="red",pch=20)
  }
  restab[[h]] <- restab_temp
}

#layer 3
for(h in c(6:9)){
  target <- colnames(foretab[[h]])[which(grepl("Ilag.HHS",colnames(foretab[[h]])))]
  target <- data.frame(matrix(unlist(strsplit(gsub("Ilag.HHS","",target),"[.]")),ncol=2,byrow=T))
  colnames(target) <- c("Region","Lag")
  target["Location"] <- which(grepl("Ilag.HHS",colnames(foretab[[h]])))
  for(sim in 1:nsim){
    restab_temp[[sim]] <- foretab[[h]]
    for(var in 1:nrow(target)){
      restab_temp[[sim]][,target$Location[var]] <- lags(restab[[target$Region[var]]][[sim]]$y,as.numeric(as.character(target$Lag[var])))
    }
    print(any(is.na(tail(restab_temp[[sim]][,target$Location],52))))
    
    repeat{
      pred <- predict(model.FN[[h]],newdata=restab_temp[[sim]],se=T) 
      pred_mean <- pred$fit[!is.na(pred$fit)]
      pred_mean <- pred_mean[length(pred_mean)]
      pred_se <- pred$se.fit[!is.na(pred$se.fit)]
      forecast.togo <- length(which(is.na(restab_temp[[sim]]$y)))
      pred_se <- sample(pred_se,1)*sqrt(forecast.togo)#pred_se[length(pred_se)]##*multiplier[week_in]#
      new_val <- rnorm(1,mean=pred_mean,sd=pred_se)
      for (col in c(ncol(restab_temp[[sim]]),which(grepl("Ilag",colnames(restab_temp[[sim]]))))){ 
        restab_temp[[sim]][is.na(restab_temp[[sim]][,col]),col][1:length(new_val)] <- new_val}
      if (all(!is.na(restab_temp[[sim]]$y))) {break}
    }
    plot(tail(exp(restab_temp[[sim]]$y),52))
    points(y=tail(exp(restab_temp[[sim]]$y),52-week_in),x=tail(1:52,52-week_in),col="red",pch=20)
  }
  restab[[h]] <- restab_temp
}

#layer 4
for(h in 10){
  target <- colnames(foretab[[h]])[which(grepl("Ilag.HHS",colnames(foretab[[h]])))]
  target <- data.frame(matrix(unlist(strsplit(gsub("Ilag.HHS","",target),"[.]")),ncol=2,byrow=T))
  colnames(target) <- c("Region","Lag")
  target["Location"] <- which(grepl("Ilag.HHS",colnames(foretab[[h]])))
  for(sim in 1:nsim){
    restab_temp[[sim]] <- foretab[[h]]
    for(var in 1:nrow(target)){
      restab_temp[[sim]][,target$Location[var]] <- lags(restab[[target$Region[var]]][[sim]]$y,as.numeric(as.character(target$Lag[var])))
    }
    print(any(is.na(tail(restab_temp[[sim]][,target$Location],52))))
    
    repeat{
      pred <- predict(model.FN[[h]],newdata=restab_temp[[sim]],se=T) 
      pred_mean <- pred$fit[!is.na(pred$fit)]
      pred_mean <- pred_mean[length(pred_mean)]
      pred_se <- pred$se.fit[!is.na(pred$se.fit)]
      forecast.togo <- length(which(is.na(restab_temp[[sim]]$y)))
      pred_se <- sample(pred_se,1)*sqrt(forecast.togo)#pred_se[length(pred_se)]##*multiplier[week_in]#
      new_val <- rnorm(1,mean=pred_mean,sd=pred_se)
      for (col in c(ncol(restab_temp[[sim]]),which(grepl("Ilag",colnames(restab_temp[[sim]]))))){ 
        restab_temp[[sim]][is.na(restab_temp[[sim]][,col]),col][1:length(new_val)] <- new_val}
      if (all(!is.na(restab_temp[[sim]]$y))) {break}
    }
    plot(tail(exp(restab_temp[[sim]]$y),52))
    points(y=tail(exp(restab_temp[[sim]]$y),52-week_in),x=tail(1:52,52-week_in),col="red",pch=20)
  }
  restab[[h]] <- restab_temp
}

#setwd("~/Google Drive/Influenza/16-17_forecast/Environmental+Functional")
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental+Functional")
tot_var <- read.csv("tot_var.csv",stringsAsFactors = F)
hist_min <- sapply(4:13, function(y) sapply(1:19,function(x) max(as.numeric(as.character(tot_var[tot_var$season==x,y])),na.rm=T)))

#setwd("~/Google Drive/Influenza/16-17_forecast/Environmental+Functional/Weekly_Forecasts_Plots/Report5")
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental+Functional/Weekly_Forecasts_Plots/Report6")#
rawres <- list()
for(h in 1:10){
  temp <- data.frame(tail(exp(sapply(restab[[h]], "[[","y")),52))
  temp <- temp[,!(apply(temp,2,which.max)>26)]
  temp <- temp[,!apply(temp,2, function(x) any(max(x)<(min(hist_min[,h]))*0.8))]
  temp <- temp[,!apply(temp,2, function(x) any(max(x)>(max(hist_min[,h]))*1.2))]
  rawres[[h]] <- as.data.frame(temp)
  rm(temp)
  
  #setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental+Functional/Weekly_Forecasts_Plots/Report5")
  png.name <- paste(paste(paste("HHS",h,sep=""),submission_date[report],sep="-"),"-4wk.png",sep="")
  png(png.name)
  plot(1,xlab="Epidemic Week (Starting from Week 40 Calendar Year)",ylab="ILI%",ylim=range(unlist(rawres[[h]][1:(week_in+4),])),type="n",xlim=c(1,week_in+4),main=paste("EW",40+week_in-1,sep=""))
  
  points(x=jitter(rep(week_in+1,ncol(rawres[[h]]))),y=rawres[[h]][(week_in+1),],col=alpha("grey",0.5),pch=20)
  points(x=jitter(rep(week_in+2,ncol(rawres[[h]]))),y=rawres[[h]][(week_in+2),],col=alpha("grey",0.5),pch=20)
  points(x=jitter(rep(week_in+3,ncol(rawres[[h]]))),y=rawres[[h]][(week_in+3),],col=alpha("grey",0.5),pch=20)
  points(x=jitter(rep(week_in+4,ncol(rawres[[h]]))),y=rawres[[h]][(week_in+4),],col=alpha("grey",0.5),pch=20)
  points(head(rawres[[h]][,1],week_in),col="green",pch=20)
  points(y=rowMeans(rawres[[h]][(week_in+1):(week_in+4),]),x=c((week_in+1):(week_in+4)),pch=20,col="red")
  lines(head(rawres[[h]][,1],week_in),col="green",lwd=2)
  lines(y=c(tail(head(rawres[[h]][,1],week_in),1),rowMeans(rawres[[h]][(week_in+1):(week_in+4),])),x=c((week_in):(week_in+4)),col="red",lty=2,lwd=2)
  abline(h=baseline[h+1,12], lty=2)
  dev.off()
}

#setwd("~/Google Drive/Influenza/16-17_forecast/Environmental+Functional/rawResults")
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental+Functional/rawResults")
file_name <- paste("rawResults_EW",40+week_in-1,".RData",sep="")
save(rawres,file=file_name)
