setwd("~/Google Drive/Influenza/16-17_forecast")
submission <- read.csv("Long_Flu_Submission_Template_update.csv")
source("draw.wk.sim.R");source("draw.onsettime.sim.R"); source("draw.peakweek.sim.R");source("draw.peakweek.sim.R")

baseline <- read.csv("wILI_Baseline.csv",stringsAsFactors = F)
epi_weeks <- c(40:52,1:39)
submission_date <- seq(as.Date("2016-11-07"),as.Date("2017-05-15"),7)
report = 5
week_in=8

setwd("~/Google Drive/Influenza/16-17_forecast/Environmental+Functional/rawResults")
all_files <- list.files(pattern="rawResults_EW")
load(all_files[length(all_files)])

for(h in 1:10){
  temp <- (rawres[[h]])
  peaksize <- peaktime <- onsettime <- rep(NA,ncol(temp))
  for(i in 1:ncol(temp)){
    peaksize[i] <- max(temp[,i])
    peaktime[i] <- which.max(temp[,i])
    onsettime_temp <- (intersect(intersect(which(temp[,i]>baseline[h+1,12]),which(temp[,i]>baseline[h+1,12])+1),which(temp[,i]>baseline[h+1,12])+2)-2)
    if (length(onsettime_temp)!=0)  {onsettime[i] <- min(onsettime_temp)} else {onsettime[i] <- NA}
    }
  
  res <- c(epi_weeks[round(mean(onsettime,na.rm=T))],draw.onset.sim(onsettime)[,2],
           epi_weeks[round(mean(peaktime))],draw.peakweek.sim(peaktime)[,2],
           mean(peaksize),draw.peak.sim(peaksize)[,3],
           mean(unlist(temp[week_in+1,])),draw.wk.sim((unlist(temp[week_in+1,])))[,3],
           mean(unlist(temp[week_in+2,])),draw.wk.sim((unlist(temp[week_in+2,])))[,3],
           mean(unlist(temp[week_in+3,])),draw.wk.sim((unlist(temp[week_in+3,])))[,3],
           mean(unlist(temp[week_in+4,])),draw.wk.sim((unlist(temp[week_in+4,])))[,3]
  )
  
  name <- paste("HHS Region",h)
  submission[submission$Location==name,"Value"] <- res
}

file_name <- paste(paste(paste(paste("EW",40+week_in-1,sep=""),"HumNat3",submission_date[report],sep="-"),".csv",sep=""))
setwd("~/Google Drive/Influenza/16-17_forecast/Environmental+Functional/Submissions")
write.csv(submission,file_name)
