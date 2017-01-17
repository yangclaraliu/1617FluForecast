require(dlnm);require(splines);require(infotheo);require(usdm); require(randomForest);require(zoo)
#load("~/Google Drive/Influenza/16-17_forecast/HHS_timealign3.RData")
load("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/HHS_timealign4.RData")
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression")
#setwd("~/Google Drive/Influenza/16-17_forecast/Environmental_Regression")
source("lags.R")
model_HHS <- list()
lags_HHS <- list()

for(h in 1:10){
  HHS_flu <- HHS_regtab[[h]]
  start <- which(HHS_flu$wk==40)
  end <- c(which(HHS_flu$wk==39),nrow(HHS_flu))
  HHS_flu["season"]<-NA;for(i in 1:length(start)){HHS_flu$season[start[i]:end[i]]<-i}
  HHS_flu <- HHS_flu[HHS_flu$season>=6 & HHS_flu$season <=19,]
  HHS_flu[HHS_flu$ILIp==0,"ILIp"] <- NA
  # MI_Tmean <- rep(NA,52); for(i in 1:52) {MI_Tmean[i] <- mutinformation(discretize(HHS_flu$ILIp)$X,lags(discretize(HHS_flu$wm_TS_mean)$X,i))}
  # plot(MI_Tmean)
  # which.max(MI_Tmean)
  # 
  # MI_ILI <- rep(NA,104);for(i in 3:104) {MI_ILI[i] <- mutinformation(discretize(HHS_flu$ILIp)$X,lags(discretize(HHS_flu$ILIp)$X,i))}
  # plot(MI_ILI)
  # 
  # MI_Tv <- rep(NA,52); for(i in 1:52) {MI_Tv[i] <- mutinformation(discretize(HHS_flu$ILIp)$X,lags(discretize(HHS_flu$wsd_TS_mean)$X,i))}
  # plot(MI_Tv)
  # which.max(MI_Tv[15:45])
  # 
  # MI_Hmean <- rep(NA,52); for(i in 1:52) {MI_Hmean[i] <- mutinformation(discretize(HHS_flu$ILIp)$X,lags(discretize(HHS_flu$wm_QV2M_mean)$X,i))}
  # plot(MI_Hmean)
  # which.max(MI_Hmean)
  # 
  #MI_Hv <- rep(NA,52); for(i in 1:52) {MI_Hv[i] <- mutinformation(discretize(HHS_flu$ILIp)$X,lags(discretize(HHS_flu$wsd_QV2M_mean)$X,i))}
  #plot(MI_Hv)
  # which.max(MI_Hv[1:40])
  regtab <- HHS_flu
  Ivar <- c(1:4,25,29,52)
  Tvar <- c(1:3,24:28)
  TVvar <- c(1:4)
  Hvar <- c(1:3,26:28)
  HVvar <- c(24:26)
  regtab[,c(paste("Ilag",Ivar,sep=""))] <- sapply(Ivar,function(x) lags(log(HHS_flu$ILIp),x))
  regtab[,c(paste("Tlag",Tvar,sep=""))] <- sapply(Tvar,function(x) lags(HHS_flu$wm_TS_mean,x))
  regtab[,c(paste("TVlag",TVvar,sep=""))] <- sapply(TVvar,function(x) lags(HHS_flu$wsd_TS_mean,x))
  regtab[,c(paste("Hlag",Hvar,sep=""))] <- sapply(Hvar,function(x) lags(HHS_flu$wm_QV2M_mean,x))
  regtab[,c(paste("HVlag",HVvar,sep=""))] <- sapply(HVvar,function(x) lags(HHS_flu$wsd_QV2M_mean,x))
  regtab_onlypredictor <- regtab[,c((ncol(HHS_flu)+1):ncol(regtab))]
  regtab_vif <- vifstep(regtab_onlypredictor,th=5)
  regtab_aftervif <- regtab_onlypredictor[,setdiff(regtab_vif@variables,regtab_vif@excluded)]
  regtab_aftervif['y'] <- log(regtab$ILIp)
  regtab_rf <- randomForest(y~.,regtab_aftervif,importance=T,na.action = na.omit)
  regtab_importance <- importance(regtab_rf); regtab_importance <- regtab_importance[order(regtab_importance[,1],decreasing=T),]
  regtab_aftervif <- regtab_aftervif[,which(colnames(regtab_aftervif)%in% rownames(regtab_importance)[which(regtab_importance[,1]>4)])]
  regtab_aftervif['y'] <- log(regtab$ILIp)
  seasonality <- stl(ts(regtab$ILIp,freq=52),s.window = "periodic",na.action = na.approx); seasonality <- seasonality$time.series[,1]
  plot(seasonality)
  regtab_aftervif["seasonality"] <- seasonality
  model <- glm(y~.,family="gaussian",data=regtab_aftervif)
  plot(regtab$ILIp,type='l')
  lines(c(rep(NA,52),exp(model$fitted.values)),col="red")
  model_HHS[[h]] <- model
  Ivar <- as.numeric(gsub("Ilag","",names(coefficients(model))[grep("Ilag",names(coefficients(model)))]))
  Tvar <- as.numeric(gsub("Tlag","",names(coefficients(model))[grep("Tlag",names(coefficients(model)))]))
  Hvar <- as.numeric(gsub("Hlag","",names(coefficients(model))[grep("Hlag",names(coefficients(model)))]))
  TVvar <- as.numeric(gsub("TVlag","",names(coefficients(model))[grep("TVlag",names(coefficients(model)))]))
  HVvar <- as.numeric(gsub("HVlag","",names(coefficients(model))[grep("HVlag",names(coefficients(model)))]))
  lags_HHS[[h]] <- list(Ivar,Tvar,Hvar,TVvar,HVvar)
}

save(model_HHS, file="Model_HHS3.RData")
save(lags_HHS, file="lags_HHS3.RData")
