require(cdcfluview)
HHS <- get_flu_data("hhs",1,"ilinet",c(2002:2016))
start <- which(HHS$WEEK==40)
end <- c(which(HHS$WEEK==39),nrow(HHS))
HHS["season"] <- NA
for(i in 1:15) {HHS$season[(start[i]:end[i])] <- i}

N <- HHS[HHS$season==5,"X..WEIGHTED.ILI"]
hist(N)
N <- cumsum(N)
N <- approx(smooth.spline(N),n = 52*28)$y
G <- rep(NA,length(N))
for(i in 1:length(N)){
  if(i ==1) {G[i] <- (N[i+1]-N[i])/2}
  else if(i == length(N)) {G[i] <- (N[i]-N[i-1])/2}
  else {G[i] <- (N[i+1]-N[i-1])/2}
}

####################
M <- (max(G))
N0 <- (tail(N,1))
G_N <- (4*M/(N0^2))*N*(N0-N)
plot(x=N,y=G,type='l')
lines(y=G_N,x=N,col='red',lwd=2)
