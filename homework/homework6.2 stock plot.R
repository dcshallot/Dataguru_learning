
#下载Apple、Google、微软三家股票数据，计算它们从2008年到2011年每年的总成交量。
#然后画一堆叠方式的直方图，横坐标上标注股票名，直方图描述每一年成交量占总成交量的百分比
#这个百分比要标注在直方图内部，颜色代表不同的年份（用图例说明） 

library(quantmod)
library(tseries)

aapl<- get.hist.quote(instrument = "aapl", quote =  "Vol", start="2008-01-01", end="2011-12-31")
goog <- get.hist.quote(instrument = "goog", quote = "Vol", start="2008-01-01", end="2011-12-31")
msft <- get.hist.quote(instrument = "msft", quote = "Vol", start="2008-01-01", end="2011-12-31")

ap <- as.numeric( Vo( to.yearly(aapl)))
go <- as.numeric( Vo( to.yearly(goog)))
ms <- as.numeric( Vo( to.yearly(msft)))

stock <- data.frame(yr=c("2008" , "2009" , "2010", "2011") ,  
                    apple = ap/sum(ap), 
                    google = go/sum(go), 
                    microsoft = ms/sum(ms))


x <- barplot( as.matrix( stock[,-1]) , beside= T, col = rainbow(4), ylim=c(0,0.5) )
y <- c(  ap/sum(ap), go/sum(go),  ms/sum(ms) )/2
text( x, y , paste( round(y *100), "%", sep="" ), cex = 0.6 )
legend( "topright", legend=stock$yr, horiz=T, bty="n" , cex = 0.8, pch=5, col= rainbow(4) )
