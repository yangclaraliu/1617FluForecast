#load("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression/Model_HHS2.RData")
#
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental+Functional/tot_var")
record_list <- list.files(pattern=".RData")
load(record_list[length(record_list)])

 # tot_var <- read.csv("tot_var_EW47.csv",stringsAsFactors = F)
load("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental+Functional/M2_models.RData")
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental+Functional")

#setwd("~/Google Drive/Influenza/16-17_forecast/Environmental+Functional/tot_var")
#tot_var <- read.csv("tot_var_EW47.csv",stringsAsFactors = F)
# load("~/Google Drive/Influenza/16-17_forecast/Environmental+Functional/M2_models.RData")
# setwd("~/Google Drive/Influenza/16-17_forecast/Environmental+Functional")

tot_var <- tot
source("lags.R")

#prepare new segment
current_wk <- tail(tot_var$wk,1)
week_in <- current_wk-40+1
epi_weeks <- c(40:52,1:39)
new_seg <- data.frame(matrix(NA,ncol=ncol(tot_var),nrow=52-week_in))
colnames(new_seg) <- colnames(tot_var)
new_seg$wk <- epi_weeks[(which(epi_weeks==current_wk)+1):length(epi_weeks)]
for(i in 1:nrow(new_seg)){if(new_seg$wk[i]>=40) {new_seg$yr[i] <- 2016} else {new_seg$yr[i] <- 2017}}
new_seg$season <- 20
tot <- rbind(tot_var,new_seg)

#fill in environmental portion
for(i in which(is.na(tot$HHS10_hsd))){
  tot[i,c(14:53)] <- colMeans(sapply((dplyr::sample_n(tot_var[tot_var$wk==tot$wk[i],],9)[,c(14:53)]),as.numeric),na.rm=T)
}
#

#data store
foretab <- list()

#draw region based forecast table, old format
for(h in c(1,3,4)){
  var <- names(model.FN[[h]]$coefficients)[grepl("lag",names(model.FN[[h]]$coefficients))]
  var <- matrix(unlist(strsplit(var,"lag")),ncol=2,byrow=T)
  
  I_seg <- data.frame(matrix(NA,ncol=length(which(var[,1]=="I")),nrow=nrow(tot)))
  for(i in 1:ncol(I_seg)){ I_seg[,i] <- lags(log(as.numeric(as.character(tot[,which(colnames(tot_var)==paste("HHS",h,"_inc",sep=""))]))),as.numeric(var[var[,1]=="I",2])[i])}
  regtab_temp <- I_seg
  regtab_temp <- cbind(tot[,1:3],regtab_temp)
  
  if(length(which(var[,1]=="T"))>=1){
    T_seg <- data.frame(matrix(NA,ncol=length(which(var[,1]=="T")),nrow=nrow(tot)))
    for(i in 1:ncol(T_seg)){ T_seg[,i] <- lags(as.numeric(as.character(tot[,which(colnames(tot_var)==paste("HHS",h,"_tmean",sep=""))])),as.numeric(var[var[,1]=="T",2])[i])}
  } else {T_seg <- NULL}
  if(!is.null(T_seg)){regtab_temp <- cbind(regtab_temp,T_seg)}
  
  if(length(which(var[,1]=="TV"))>=1){
    TV_seg <- data.frame(matrix(NA,ncol=length(which(var[,1]=="TV")),nrow=nrow(tot)))
    for(i in 1:ncol(TV_seg)){TV_seg[,i] <- lags(as.numeric(as.character(tot[,which(colnames(tot_var)==paste("HHS",h,"_tsd",sep=""))])),as.numeric(var[var[,1]=="TV",2])[i])}
  } else {TV_seg <- NULL}
  if(!is.null(TV_seg[1])){regtab_temp <- cbind(regtab_temp,TV_seg)}
  
  if(length(which(var[,1]=="H"))>=1){
    H_seg <- data.frame(matrix(NA,ncol=length(which(var[,1]=="H")),nrow=nrow(tot)))
    for(i in 1:ncol(H_seg)){ H_seg[,i] <- lags(as.numeric(as.character(tot[,which(colnames(tot_var)==paste("HHS",h,"_hmean",sep=""))])),as.numeric(var[var[,1]=="H",2])[i])}
  }else {H_seg <- NULL}
  if(!is.null(H_seg)){regtab_temp <- cbind(regtab_temp,H_seg)}
  
  if(length(which(var[,1]=="HV"))>=1){
    HV_seg <- data.frame(matrix(NA,ncol=length(which(var[,1]=="HV")),nrow=nrow(tot)))
    for(i in 1:ncol(HV_seg)){HV_seg[,i] <- lags(as.numeric(as.character(tot[,which(colnames(tot_var)==paste("HHS",h,"_hsd",sep=""))])),as.numeric(var[var[,1]=="HV",2])[i])}
  }else {HV_seg <- NULL}
  if(!is.null(HV_seg)){regtab_temp <- cbind(regtab_temp,HV_seg)}
  
  colnames(regtab_temp)[4:ncol(regtab_temp)] <- names(model.FN[[h]]$coefficients)[grepl("lag",names(model.FN[[h]]$coefficients))]
  
  if(any(names(model.FN[[h]]$coefficients)=="seasonality")){
    temp <- as.numeric(as.character(tot[,which(colnames(tot_var)==paste("HHS",h,"_inc",sep=""))]))
    temp <- temp[min(which(tot$season==6)):length(temp)]
    temp <- temp[!is.na(temp)]
    regtab_temp[(min(which(tot$season==6)):(min(which(tot$season==6))+length((stl(ts(temp,freq=52),s.window="periodic")$time.series[,1]))-1)),"seasonality"] <- (stl(ts(temp,freq=52),s.window="periodic")$time.series[,1])
    regtab_temp[regtab_temp$season==20,"seasonality"] <- (stl(ts(temp,freq=52),s.window="periodic")$time.series[,1])[1:52]
  }
  regtab_temp["y"] <- log(as.numeric(as.character(tot[,which(colnames(tot_var)==paste("HHS",h,"_inc",sep=""))])))
  regtab_temp <- regtab_temp[regtab_temp$season>=6,]
  foretab[[h]] <- regtab_temp#[,4:ncol(regtab_temp)]
}

#draw region based forecast table, new format
for(h in c(2,5:10)){
  var_local <- names(model.FN[[h]]$coefficients)[grepl("lag.",names(model.FN[[h]]$coefficients))]
  var_ld <- names(model.FN[[h]]$coefficients)[grepl("lag.HHS",names(model.FN[[h]]$coefficients))]
  var_local <- var_local[!(var_local %in% var_ld)]
  var_local <- matrix(unlist(strsplit(var_local,"lag.")),ncol=2,byrow=T)
  
  #put together local parameters
  I_seg <- data.frame(matrix(NA,ncol=length(which(var_local[,1]=="I")),nrow=nrow(tot)))
  for(i in 1:ncol(I_seg)){ I_seg[,i] <- lags(log(as.numeric(as.character(tot[,which(colnames(tot_var)==paste("HHS",h,"_inc",sep=""))]))),as.numeric(var_local[var_local[,1]=="I",2])[i])}
  regtab_temp <- I_seg
  regtab_temp <- cbind(tot[,1:3],regtab_temp)
  
  if(length(which(var_local[,1]=="T"))>=1){
    T_seg <- data.frame(matrix(NA,ncol=length(which(var_local[,1]=="T")),nrow=nrow(tot)))
    for(i in 1:ncol(T_seg)){ T_seg[,i] <- lags(as.numeric(as.character(tot[,which(colnames(tot_var)==paste("HHS",h,"_tmean",sep=""))])),as.numeric(var_local[var_local[,1]=="T",2])[i])}
  } else {T_seg <- NULL}
  if(!is.null(T_seg)){regtab_temp <- cbind(regtab_temp,T_seg)}
  
  if(length(which(var_local[,1]=="H"))>=1){
    H_seg <- data.frame(matrix(NA,ncol=length(which(var_local[,1]=="H")),nrow=nrow(tot)))
    for(i in 1:ncol(H_seg)){ H_seg[,i] <- lags(as.numeric(as.character(tot[,which(colnames(tot_var)==paste("HHS",h,"_hmean",sep=""))])),as.numeric(var_local[var_local[,1]=="H",2])[i])}
  }else {H_seg <- NULL}
  if(!is.null(H_seg)){regtab_temp <- cbind(regtab_temp,H_seg)}
  
  if(length(which(var_local[,1]=="TV"))>=1){
    TV_seg <- data.frame(matrix(NA,ncol=length(which(var_local[,1]=="TV")),nrow=nrow(tot)))
    for(i in 1:ncol(TV_seg)){TV_seg[,i] <- lags(as.numeric(as.character(tot[,which(colnames(tot_var)==paste("HHS",h,"_tsd",sep=""))])),as.numeric(var_local[var_local[,1]=="TV",2])[i])}
  } else {TV_seg <- NULL}
  if(!is.null(TV_seg[1])){regtab_temp <- cbind(regtab_temp,TV_seg)}
  
  if(length(which(var_local[,1]=="HV"))>=1){
    HV_seg <- data.frame(matrix(NA,ncol=length(which(var_local[,1]=="HV")),nrow=nrow(tot)))
    for(i in 1:ncol(HV_seg)){HV_seg[,i] <- lags(as.numeric(as.character(tot[,which(colnames(tot_var)==paste("HHS",h,"_hsd",sep=""))])),as.numeric(var_local[var_local[,1]=="HV",2])[i])}
  }else {HV_seg <- NULL}
  if(!is.null(HV_seg)){regtab_temp <- cbind(regtab_temp,HV_seg)}
  
  #put together long distance parameters
  var_ld <- matrix(unlist(strsplit(gsub("Ilag.","",var_ld),split="[.]")),ncol=2,byrow=T)
  Ild_seg <- matrix(NA,ncol=nrow(var_ld),nrow=nrow(regtab_temp))
  for(i in 1:ncol(Ild_seg)){
    Ild_seg[,i] <- lag(log(as.numeric(as.character((tot[,which(colnames(tot)==paste(var_ld[i],"_inc",sep=""))])))),as.numeric(var_ld[i,2]))}
  regtab_temp <- cbind(regtab_temp,Ild_seg)
  
  if(any(names(model.FN[[h]]$coefficients)=="seasonality")){
    temp <- as.numeric(as.character(tot[,which(colnames(tot_var)==paste("HHS",h,"_inc",sep=""))]))
    temp <- temp[min(which(tot$season==6)):length(temp)]
    temp <- temp[!is.na(temp)]
    regtab_temp[(min(which(tot$season==6)):(min(which(tot$season==6))+length((stl(ts(temp,freq=52),s.window="periodic")$time.series[,1]))-1)),"seasonality"] <- (stl(ts(temp,freq=52),s.window="periodic")$time.series[,1])
    regtab_temp[regtab_temp$season==20,"seasonality"] <- (stl(ts(temp,freq=52),s.window="periodic")$time.series[,1])[1:52]
  }
  
  
  colnames(regtab_temp)[4:ncol(regtab_temp)] <- names(coef(model.FN[[h]]))[-1]
  regtab_temp["y"] <- log(as.numeric(as.character(tot[,which(colnames(tot_var)==paste("HHS",h,"_inc",sep=""))])))
  regtab_temp <- regtab_temp[regtab_temp$season>=6,]
  foretab[[h]] <- regtab_temp#[4:ncol(regtab_temp)]
}

setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental+Functional/foretab")
#setwd("~/Google Drive/Influenza/16-17_forecast/Environmental+Functional/foretab")
save(foretab,file=paste("foretab_EW",current_wk,".RData",sep=""))
