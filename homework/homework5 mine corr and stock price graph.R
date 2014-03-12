library(rJava)
# MINE correlation
setwd("D:/github/dataguru_learningR/homework/")
source("MINE.r")
WHO <- read.csv("WHO.csv")

#MINE( "WHO.csv","one.pair",4,8)
#rMINE( as.matrix(WHO[,-1]) ,"MICout" , "all.pairs") #mine starts from 0
MINE( "WHO.csv" , "pairs.between",220, 350 ) #mine starts from 0
mic <- read.csv("WHO.csv,between[break=220],cv=0.0,B=n^0.6,Results.csv")
mic.h <- mic[, c(1:5,10)]
names(mic.h) <- c("x.var","x.id","y.var","y.id","mic", "lg")
plot(mic.h$mic, mic.h$r)
corr <- mic.h[ mic.h$mic ==1 , c(2,4,5)]
rr <- vector()
for ( i in 1:nrow(corr))  {
  rr[i] <- cor( WHO[ ,corr[i,1]], WHO[,corr[i,2]] , "pairwise.complete.obs")
}
corr <- cbind( corr, rr)
corr <- corr[ !is.na(corr$rr) ,]
corr[ abs(corr$rr) < 0.5,]
plot( WHO[,70] , WHO[, 325] , 
      xlab= "Births.attended.by.skilled.health.....wealth", 
      ylab= "energy_consumption")


#grab oracle

library(quantmod)
library(tseries)

orcl <- get.hist.quote(instrument = "ORCL", quote = c("Cl", "Vol"))

plot(orcl$Close,main = "Stock Price of Cracle",
     ylim=c(0,150), col="red", type="l", lwd=0.5,
     pch=19,cex=0.6, xlab="Date" ,ylab="Stock Price (USD)")

text( locator(1), paste( "high", max(orcl$Close)) )
text( locator(1), paste( "low", min(orcl$Close)) )


