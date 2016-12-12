setwd("C:/Users/liux3204/Google Drive/Influenza/16-17_forecast/Environmental+Functional")
require(infotheo); require(usdm);require(randomForest)
#setwd("~/Google Drive/Influenza/16-17_forecast/Environmental+Functional")
source("lags.R")
tot_var <- read.csv("tot_var.csv",stringsAsFactors = F)
load("lags_HHS2.RData")
load("model_HHS2.RData")
h_list <- c(1,2,3,4,5,6,7,8,9,10)
links.HHS_list1 <- list(NA,
                       c(1),
                       NA,
                       NA,
                       c(1,3,4),                  
                       c(5),
                       c(4),
                       c(5),
                       c(4),
                       c(5))

links.HHS_list2 <- list(NA,
                        c(1),
                        NA,
                        NA,
                        c(1,3,4),                  
                        c(5),
                        c(2,4),
                        c(4,5),
                        c(1,2,4),
                        c(3,5,6,9))

links.HHS_list3 <- list(NA,
                        c(1),
                        c(2),
                        c(2),
                        c(1,3,4),                  
                        c(2,5),
                        c(2,4),
                        c(2,4,5),
                        c(1,2,4),
                        c(2,3,5,6,7,9))

model.FN <- list()
for(h in 1:10){
  if(is.na(links.HHS_list2[[h]][1])){model.FN[[h]] <- model_HHS[[h]]} else {
    links.HHS <- links.HHS_list2[[h]]
    vartab.HHS <- tot_var[,sort(c(1:3,which(grepl(paste("HHS",h,sep=""),(colnames(tot_var)))==T),which(colnames(tot_var)%in%paste("HHS",links.HHS,"_inc",sep=""))))]
    vartab.HHS <- vartab.HHS[vartab.HHS$season>=6,]
    vartab.HHS <- as.data.frame(apply(vartab.HHS,2,as.numeric))
    max(which(grepl("_inc",colnames(vartab.HHS))))
    
    colnames(vartab.HHS)[(min(which(grepl("_inc",colnames(vartab.HHS))))):ncol(vartab.HHS)] <- c(paste("I.HHS",links.HHS,sep=""),"I","T","TV","H","HV")
    
    #look for relationships
    inc_mutinfo <- list()
    for(neighbor in 1:length(links.HHS)){
      inc_mutinfo[[neighbor]] <- rep(NA,52)
      for(l in 1:52) {
        inc_mutinfo[[neighbor]][l] <- mutinformation(lags(discretize(vartab.HHS[,(4+neighbor-1)],nbins=10)$X,l), discretize(vartab.HHS$I,nbins=10)$X)
      }
    }
    
    if(length(links.HHS)%%2==0){
      par(mfrow=c(length(links.HHS),2))
      for(i in 1:length(inc_mutinfo)){
        plot(inc_mutinfo[[i]],main=links.HHS[i])
      }
    } else {
      par(mfrow=c((length(links.HHS)+1)/2,2))
      for(i in 1:length(inc_mutinfo)){
        plot(inc_mutinfo[[i]],main=links.HHS[i])
      }
    }
    
    I.HHSvar.lag <- list()
    for(i in 1:length(links.HHS)){I.HHSvar.lag[[i]] <- c(1,4)}
    
    #import environmental model
    Ivar <- lags_HHS[[h]][[1]]
    Tvar <- lags_HHS[[h]][[2]]
    Hvar <- lags_HHS[[h]][[3]]
    TVvar <- lags_HHS[[h]][[4]]
    HVvar <- lags_HHS[[h]][[5]]
    
    #create column names
    names <- c(paste("Ilag.",Ivar,sep=""))
    if(length(Tvar>0)){names <- c(names,paste("Tlag.",Tvar,sep=""))}
    if(length(Hvar>0)){names <- c(names,paste("Hlag.",Hvar,sep=""))}
    if(length(TVvar>0)){names <- c(names,paste("TVlag.",TVvar,sep=""))}
    if(length(HVvar>0)){names <- c(names,paste("HVlag.",HVvar,sep=""))}
    names <- c(names,paste(paste("Ilag.HHS",rep(links.HHS,each=length(I.HHSvar.lag[[1]])),sep=""),".",rep(I.HHSvar.lag[[1]],length(links.HHS)),sep=""))
    names
    
    #create lag tables
    I_seg <- data.frame((sapply(Ivar,function(x) lags(log(vartab.HHS$I),x))))
    T_seg <- data.frame((sapply(Tvar,function(x) lags(vartab.HHS$T,x))))
    H_seg <- data.frame((sapply(Hvar,function(x) lags(vartab.HHS$H,x))))
    TV_seg <- data.frame((sapply(TVvar,function(x) lags(vartab.HHS$TV,x))))
    HV_seg <- data.frame((sapply(HVvar,function(x) lags(vartab.HHS$HV,x))))
    
    I.HHS_seg <- list()
    for(i in 1:length(links.HHS)){
      temp_df <- data.frame(matrix(NA,ncol=length(I.HHSvar.lag[[1]]),nrow=nrow(vartab.HHS)))
      for(c in 1:ncol(temp_df))  temp_df[,c] <- lags(log(vartab.HHS[,3+i]),I.HHSvar.lag[[i]][c])
      I.HHS_seg[[i]] <- temp_df
      rm(temp_df)
    }
    
    #create table
    regtab.HHS.new <- vartab.HHS[,1:3]
    regtab.HHS.new["y"] <- log(vartab.HHS$I)
    regtab.HHS.new <- cbind(regtab.HHS.new,I_seg)
    if(dim(T_seg)[1]) { regtab.HHS.new <- cbind(regtab.HHS.new,T_seg)}
    if(dim(H_seg)[1]) { regtab.HHS.new <- cbind(regtab.HHS.new,H_seg)}
    if(dim(TV_seg)[1]) { regtab.HHS.new <- cbind(regtab.HHS.new,TV_seg)}
    if(dim(HV_seg)[1]) { regtab.HHS.new <- cbind(regtab.HHS.new,HV_seg)}
    for(i in 1:length(links.HHS)){
      regtab.HHS.new <- cbind(regtab.HHS.new,I.HHS_seg[[i]])
    }
    colnames(regtab.HHS.new)[5:ncol(regtab.HHS.new)] <- names
    
    for(i in 1:ncol(regtab.HHS.new)){
      if(any(is.infinite(regtab.HHS.new[,i]))){
        regtab.HHS.new[which(is.infinite(regtab.HHS.new[,i])),i] <- NA
      }
    }
    
#     #runModel
#     onlypredictor <- regtab.HHS.new[,c(5:ncol(regtab.HHS.new))]
#     onlypredictor_vif <- vifstep(onlypredictor,th=5)
#     onlypredictor_aftervif <- onlypredictor[,setdiff(onlypredictor_vif@variables,onlypredictor_vif@excluded)]
#     onlypredictor_aftervif['y'] <- log(vartab.HHS$I)
#     
#     for(i in 1:ncol(onlypredictor_aftervif)){
#       if(any(is.infinite(onlypredictor_aftervif[,i]))){
#         onlypredictor_aftervif[which(is.infinite(onlypredictor_aftervif[,i])),i] <- NA
#       }
#     }
#   
#     onlypredictor_aftervif["seasonality"] <- stl(ts(vartab.HHS[vartab.HHS$season>=6,"I"],freq=52),s.window="periodic")$time.series[,1]
    
#     onlypredictor_aftervif_rf <- randomForest(y~.,onlypredictor_aftervif,importance=T,na.action = na.omit)
#     onlypredictor_aftervif_importance <- importance(onlypredictor_aftervif_rf)
#     onlypredictor_aftervif_importance <- onlypredictor_aftervif_importance[order(onlypredictor_aftervif_importance[,1],decreasing=T),]
#     onlypredictor_aftervif_afterrf <- onlypredictor_aftervif[,which(colnames(onlypredictor_aftervif)%in% rownames(onlypredictor_aftervif_importance)[which(onlypredictor_aftervif_importance[,1]>4)])]
#     onlypredictor_aftervif_afterrf['y'] <- log(vartab.HHS$I)
#     
# #     #
#     for(i in 1:ncol(onlypredictor_aftervif_afterrf)){
#       if(any(is.infinite(onlypredictor_aftervif_afterrf[,i]))){
#         onlypredictor_aftervif_afterrf[which(is.infinite(onlypredictor_aftervif_afterrf[,i])),i] <- NA
#       }
#      }
    model.HHS<- glm(y~.,data=regtab.HHS.new[,4:ncol(regtab.HHS.new)],family = "gaussian",na.action = na.omit)
    model.FN[[h]] <-  model.HHS
  }
}

save(model.FN,file="M2_models.RData")

#model.HHS.afterrf <- glm(y~.,data=onlypredictor_aftervif_afterrf,family = "gaussian",na.action = na.omit)
