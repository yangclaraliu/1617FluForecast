setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression/fore_tab_base_all")
record_list <- list.files(pattern=".RData")
mark <- which.max(lapply(1:length(record_list),function(x) file.info(record_list[x])$ctime))
load(record_list[mark])

#load("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression/fore_tab_base_all_EW42_EN39.RData")
#which.max(gsub("EW","",matrix(unlist(strsplit(list.files(pattern = ".RData"),"_")),ncol=6,byrow=T)[,5]))
#load("~/Google Drive/Influenza/16-17_forecast/Environmental_Regression/fore_tab_base_all_EW43_EN39.RData")
#load("~/Google Drive/Influenza/16-17_forecast/Environmental_Regression/fore_tab_base_all")#/fore_tab_base_all_EW45_EN39.RData")
#setwd("~/Google Drive/Influenza/16-17_forecast/Environmental_Regression")
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression")
source("lags.R")
#load("~/Google Drive/Influenza/16-17_forecast/Environmental_Regression/lags_HHS.RData")
load("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression/lags_HHS3.RData")

#load("~/Google Drive/Influenza/16-17_forecast/Environmental_Regression/model_HHS.RData")

draw.fore.tab <- function(h){
#read lag selected
  Ivar <- lags_HHS[[h]][[1]]
  Tvar <- lags_HHS[[h]][[2]]
  Hvar <- lags_HHS[[h]][[3]]
  TVvar <- lags_HHS[[h]][[4]]
  HVvar <- lags_HHS[[h]][[5]]
#create column names
  names <- c(paste("Ilag",Ivar,sep=""))
  if(length(Tvar>0)){names <- c(names,paste("Tlag",Tvar,sep=""))}
  if(length(Hvar>0)){names <- c(names,paste("Hlag",Hvar,sep=""))}
  if(length(TVvar>0)){names <- c(names,paste("TVlag",TVvar,sep=""))}
  if(length(HVvar>0)){names <- c(names,paste("HVlag",HVvar,sep=""))}
#create lag tables
  I_seg <- data.frame((sapply(Ivar,function(x) lags(log(as.numeric(fore_tab_base_all[[h]][[1]]$ILIp)),x))))
  T_seg <- data.frame((sapply(Tvar,function(x) lags(fore_tab_base_all[[h]][[1]]$wm_TS_mean,x))))
  H_seg <- data.frame((sapply(Hvar,function(x) lags(fore_tab_base_all[[h]][[1]]$wm_QV2M_mean,x))))
  TV_seg <- data.frame((sapply(TVvar,function(x) lags(fore_tab_base_all[[h]][[1]]$wsd_TS_mean,x))))
  HV_seg <- data.frame((sapply(HVvar,function(x) lags(fore_tab_base_all[[h]][[1]]$wsd_QV2M_mean,x))))
#create table
  temp <- (I_seg)
  if(dim(T_seg)[1]) { temp <- cbind(temp,T_seg)}
  if(dim(H_seg)[1]) { temp <- cbind(temp,H_seg)}
  if(dim(TV_seg)[1]) { temp <- cbind(temp,TV_seg)}
  if(dim(HV_seg)[1]) { temp <- cbind(temp,HV_seg)}
  
  temp <- as.data.frame(temp)
  colnames(temp) <- names
  #  seasonality <- stl(ts(fore_tab_base[[1]]$ILIp,freq=52),s.window="periodic")
  temp <- cbind(fore_tab_base_all[[h]][[1]][,c("yr","wk","season","ILIp","ILIc","seasonality")],temp)
  return(temp)
}

fore_tab_all <- list()
for(h in 1:10) {fore_tab_all[[h]] <- draw.fore.tab(h)}

#setwd("~/Google Drive/Influenza/16-17_forecast/Environmental_Regression/fore_tab_all")
setwd("C:/Users/liux3204//Google Drive/Influenza/16-17_forecast/Environmental_Regression/fore_tab_all")

#
#base <- get_flu_data("national",,"ilinet",c(2002:2016))

#file_name <- paste("fore_tab_all_EW",tail(base$WEEK,1),"_EN44.RData",sep="")
file_name <- gsub("base_","",record_list[mark])
save(fore_tab_all,file=file_name)
