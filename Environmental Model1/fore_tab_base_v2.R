require(dlnm);require(splines);require(infotheo);require(cdcfluview)
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast")
record_list <- list.files(pattern=glob2rx("HHS_timealign*RData"))
load(record_list[length(record_list)])
#load("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/HHS_timealign1.RData")
#load("tot_inc.RData")
#week_in <- tail(tot_inc$wk,1)-40+1

draw.fore.tab.base <- function(h){
  HHS_flu <- HHS_regtab[[h]]
  start <- which(HHS_flu$wk==40)
  end <- c(which(HHS_flu$wk==39),nrow(HHS_flu))
  HHS_flu["season"]<-NA;for(i in 1:length(start)){HHS_flu$season[start[i]:end[i]]<-i}
  HHS_flu <- HHS_flu[HHS_flu$season>=6,]
  ILI_new <- get_flu_data("hhs",h,"ilinet",c(2002:2017))
  #ILI_new <- tot_inc[tot_inc$season>5,c(1:3,h+3)]
  week_in <- tail(ILI_new$WEEK,1)-40+1
  new_blank <- data.frame(matrix(NA,nrow=nrow(ILI_new)-nrow(HHS_flu),ncol=ncol(HHS_flu)))
  new_blank <- data.frame(matrix(NA,nrow=nrow(ILI_new)-nrow(HHS_flu),ncol=ncol(HHS_flu)))
  colnames(new_blank) <- colnames(HHS_flu)
  HHS_flu <- rbind(HHS_flu,new_blank)
  
  #HHS_flu$ILIp <- ILI_new$X..WEIGHTED.ILI
  HHS_flu$ILIp <- ILI_new$`% WEIGHTED ILI`
  HHS_flu$yr <- ILI_new$YEAR
  #HHS_flu$wk <- ILI_new$wk
  new_seg <- data.frame(matrix(NA,ncol=ncol(HHS_flu),nrow=52-week_in))
  colnames(new_seg) <- colnames(HHS_flu)
  HHS_flu <- rbind(HHS_flu,new_seg)
  rm(new_seg,new_blank)
  
  #fill in season number
  start <- which(HHS_flu$wk==40)
  end <- c(which(HHS_flu$wk==39),nrow(HHS_flu))
  HHS_flu["season"]<-NA;for(i in 1:length(start)){HHS_flu$season[start[i]:end[i]]<-i}
  
  #fill in week number
  HHS_flu["wk"]
  HHS_flu$wk[tail(which(HHS_flu$wk==40),1):nrow(HHS_flu)] <- c(40:52,1:39)
  
  #fill in year number
  HHS_flu$yr[HHS_flu$season==15 & HHS_flu$wk<=52 & HHS_flu$wk>=40] <- 2016
  HHS_flu$yr[HHS_flu$season==15 & HHS_flu$wk<=39] <- 2017
  
  #define seasonality
  seasonality <- stl(ts(HHS_flu$ILIp[!is.na(HHS_flu$ILIp)],freq=52),s.window="periodic")
  seasonality <- seasonality$time.series[,1]
  plot(seasonality)
  HHS_flu[1:length(seasonality),"seasonality"] <- seasonality
  
  #fill in unknown environment with known environment by bootstrapping
  to.fill <- which(is.na(HHS_flu$wm_TS_min))
  to.fill.season <- which(is.na(HHS_flu$seasonality))
  
  fore_tab_base <- list()
  nsim <- 1000
  for (sim in 1:nsim){
    for(i in to.fill){
      HHS_flu[i,7:22] <- colMeans(HHS_flu[sample(rev(rev(which(HHS_flu$wk==HHS_flu[i,"wk"]))[-1]),3),7:22])
    }
    for(i in to.fill.season){
      HHS_flu[i,23] <- mean(HHS_flu[sample(rev(rev(which(HHS_flu$wk==HHS_flu[i,"wk"]))[-1]),3),23])
    }
    fore_tab_base[[sim]] <- HHS_flu
  }
  return(fore_tab_base)
}

base <- get_flu_data("national",,"ilinet",c(2002:2016))

fore_tab_base_all <- list()
for(h in 1:10){fore_tab_base_all[[h]] <- draw.fore.tab.base(h)}
#setwd("~/Google Drive/Influenza/16-17_forecast/Environmental_Regression/fore_tab_base_all")
setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental_Regression/fore_tab_base_all")
#fill in
file_name <- paste("fore_tab_base_all_EW",tail(base$WEEK,1),"_EN44.RData",sep="")
#file_name <- paste("fore_tab_base_all_EW",tail(tot_inc$wk,1),"_EN44.RData",sep="")
save(fore_tab_base_all,file=file_name)




