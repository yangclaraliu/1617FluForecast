require(cdcfluview)
load("/Users/Yang/Google Drive/Influenza/16-17_forecast/weekly_environment7.RData")
load("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/weekly_environment7.RData")

#read environmental data
TS_mean <- sapply(1:10,function(x) weekly_mean[[x]]$TS_mean)
TS_mean <-  cbind(TS_mean,matrix(unlist(strsplit(as.character(weekly_mean[[1]]$time),split="-")),ncol=2,byrow=T))
TS_mean <- as.data.frame(apply(TS_mean,2,as.numeric))
colnames(TS_mean) <- c(paste("HHS",1:10,sep=""),"season","wk")

TS_sd <- sapply(1:10,function(x) weekly_sd[[x]]$TS_mean)
TS_sd <-  cbind(TS_sd,matrix(unlist(strsplit(as.character(weekly_mean[[1]]$time),split="-")),ncol=2,byrow=T))
TS_sd <- as.data.frame(apply(TS_sd,2,as.numeric))
colnames(TS_sd) <- c(paste("HHS",1:10,sep=""),"season","wk")

QV2M_mean <- sapply(1:10,function(x) weekly_mean[[x]]$QV2M_mean)
QV2M_mean <-  cbind(QV2M_mean,matrix(unlist(strsplit(as.character(weekly_mean[[1]]$time),split="-")),ncol=2,byrow=T))
QV2M_mean <- as.data.frame(apply(QV2M_mean,2,as.numeric))
colnames(QV2M_mean) <- c(paste("HHS",1:10,sep=""),"season","wk")

QV2M_sd <- sapply(1:10,function(x) weekly_sd[[x]]$QV2M_mean)
QV2M_sd <-  cbind(QV2M_sd,matrix(unlist(strsplit(as.character(weekly_mean[[1]]$time),split="-")),ncol=2,byrow=T))
QV2M_sd <- as.data.frame(apply(QV2M_sd,2,as.numeric))
colnames(QV2M_sd) <- c(paste("HHS",1:10,sep=""),"season","wk")

#read public health data
inc <- as.data.frame(sapply(1:10,function(x) get_flu_data("hhs",x,"ilinet",c(1997:2016))$`% WEIGHTED ILI`))
inc["yr"] <- get_flu_data("hhs",1,"ilinet",c(1997:2016))$YEAR
inc["wk"] <- get_flu_data("hhs",1,"ilinet",c(1997:2016))$WEEK
start <- which(inc$wk==40)
end <- c(which(inc$wk==39),nrow(inc))
inc["season"] <- NA
for(s in 1:length(start)){inc$season[start[s]:end[s]] <- s}
colnames(inc)[1:10] <- paste("HHS",1:10,sep="")

#align the data table
tot <- inc[,11:13]
tot[paste("HHS",1:10,"_inc",sep="")] <- inc[,1:10]
tot[paste("HHS",1:10,"_tmean",sep="")] <- TS_mean[1:nrow(tot),1:10]
tot[paste("HHS",1:10,"_tsd",sep="")] <- TS_sd[1:nrow(tot),1:10]
tot[paste("HHS",1:10,"_hmean",sep="")] <- QV2M_mean[1:nrow(tot),1:10]
tot[paste("HHS",1:10,"_hsd",sep="")] <- QV2M_sd[1:nrow(tot),1:10]

setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental+Functional/tot_var")
file_name <- paste("tot_var_EW",tail(tot$wk,1),".RData",sep="")
save(tot,file=file_name)
