

sales<-read.csv("sales.csv")
install.packages("RColorBrewer")
library(RColorBrewer)
rownames(sales)<-sales[,1]
sales<-sales[,-1]
data_matrix <- data.matrix(sales)
pal=brewer.pal(7,"YlOrRd")
breaks<-seq(3000,12000,1500)


layout(matrix(data=c(1,2),  nrow=1, ncol=2), widths=c(8,1),
       heights=c(1,1))  ## 画一个空白的图形画板，按照参数把图形区域分隔好
## 看layout的分割可以这样：
## xx <- layout(matrix(data=c(1,2),  nrow=1, ncol=2), widths=c(8,1), heights=c(1,1)) ; layout.show(xx)

par(mar = c(2,4,4,1),oma=c(0.2,0.2,0.2,0.2),mex=0.5) #Set margins for the heatmap

image(x=1:nrow(data_matrix),
      y=1:ncol(data_matrix),
      z=data_matrix,axes=FALSE, 
      xlab="Month",   ylab="", main="Sales Heat Map" ,
      col=pal[1:(length(breaks)-1)], 
      breaks=breaks ) # breaks 颜色块对应的数值（数值分组），要比颜色数量多1个

axis(1,col="white",las=1 ,
     at=1:nrow(data_matrix),
     labels=rownames(data_matrix)  )

axis(2, col="white",las=1 ,
     at=1:ncol(data_matrix),
     labels=colnames(data_matrix) )


abline(h=c(1:ncol(data_matrix))+0.5,
       v=c(1:nrow(data_matrix))+0.5,  
       col="white",lwd=2,xpd=FALSE)


# 画标尺
breaks2 <- breaks[-length(breaks)]  # breaks 少一个

#dev.off()
#par(mar = c(5,1,4,7))
par(mar = c(2,4,4,1))
image(x = 1, 
      y= 0:length(breaks2),
      z=t(matrix(breaks2))*1.001,
      col=pal[1:length(breaks)-1],
      axes=FALSE,breaks=breaks,
      xlab="", ylab="",xaxt="n")

axis(4,at=0:(length(breaks2)-1), labels=breaks2, col="white",
     las=1)
abline(h=c(1:length(breaks2)),col="white",lwd=2,xpd=F)


