setwd('D:/github/2014_Dataguru_learning_git/homework')

### google beijing map
library(maps)
library(rgdal)
library(RgoogleMaps)
library(RColorBrewer)

## 参考http://f.dataguru.cn/thread-257422-1-1.html
## 通过google地图获取北京的坐标
## 天安门广场(中心)：39.905555,116.397743
## 颐和园：40.001714,116.267323
## 通过google的路线功能手动定位沿途节点的坐标
## (39.908946,116.310711), (39.969622,116.306934)
## (39.973963,116.302299), (40.002109,116.293716)

## 构造坐标数据框
air <- data.frame(lat=c(39.905555, 39.908946, 39.969622, 39.973963, 40.002109, 40.001714),
                  lon=c(116.397743, 116.310711, 116.306934, 116.302299, 116.293716, 116.267323))

## 绘制路径和节点
beijing <- GetMap(center=c(39.916056,116.38916), 
                  zoom=11, 
                  destfile="beijing.png",
                  maptype="mobile")
PlotOnStaticMap(beijing, lat=air$lat, lon=air$lon, col=c("red"), lwd=3, FUN=lines)


### WHO
library(maps)
library(RColorBrewer)
who <- read.csv("WHO.csv")

asia <- who[ who$Continent==7
             | who$CountryID %in% c(81,88,93,94,97,109,120,144,163,177,180,198) #6
             | who$CountryID %in% c(1,21) #1 
             | who$CountryID %in% c(90,96, 178, 195 ) #2
             ,  c(1,2,5)] 

names(asia) <- c("country","cid", "literacy ")
x <- map( regions= asia$country ,plot=FALSE )
colors <- brewer.pal(7,"Reds")

for(i in 1:length(rownames(asia))) { #图形数值
  for(j in 1:length(x$names)) {
    if( grepl ( asia$country[i], x$names[j], ignore.case=T)) 
      x$measure[j]<-as.double(asia$literacy[i]) 
  }
}

sd <- data.frame(col=colors, #取色
                 values=seq(min(x$measure[!is.na(x$measure)]),
                            max(x$measure[!is.na(x$measure)])*1.0001,
                            length.out=7))
breaks <- sd$values
matchcol <- function(y) {  #颜色区间
  as.character( sd$col[ findInterval( y, sd$values)])
}

layout(matrix(data=c(2,1), nrow=1, ncol=2), widths=c(8,1), heights=c(8,1))

# Color Scale first
par(mar = c(20,1,20,7),oma=c(0.2,0.2,0.2,0.2),mex=0.5)
image(x=1, y=0:length(breaks),z=t(matrix(breaks))*1.001,
      col=colors[1:length(breaks)-1],axes=FALSE,breaks=breaks,
      xlab="", ylab="", xaxt="n") #这里画图板要拉的很大否则画不出来

axis(4,at=0:(length(breaks)-1),
     labels=round(breaks),col="white",las=1)
abline(h=c(1:length(breaks)),col="white",lwd=2,xpd=F)

#Map
map(regions= asia$country , boundary = FALSE,col=matchcol(x$measure),
    fill=TRUE,lty="blank")
map( regions= asia$country , col="white",add = TRUE)
title("Adult literacy rate of Asia")


### book

#9.1
data <- read.table("clipboard", header=T)

pca <- princomp( data[,-1], cor = T)
summary(pca, loadings=T, cutoff= 0.01)
screeplot( pca , type="lines" )
biplot( pca ,pc.biplot=F)

#从碎石图判断，1,2主成分载荷较大
#原变量中，前三变量为总量变量，4-6为效率变量，7，8为环保变量
#第一主成分中，总量变量负而效率变量为正，可看做效率因子
#第二主成分中，只有燃料消耗费为正，可看做环保因子

load <- loadings(pca)
plot(load[,1:2] ); text( load[,1], load[,2], adj=c(0.01,0.01))
#分类： 一冶金、电力、煤炭；二化学、机械、建材；三食品；四森工

#9.2
data <- read.table("clipboard", header=T)
kappa(data[,-1]) # 多重共线性

pca <- princomp( ~x1+x2+x3+x4, data=data, cor=T )
summary( pca, loadings= T)# 第一主成分贡献98%以上
pre <- predict( pca)
data$z1 <- pre[,1]
lm.p <- lm(y ~ z1, data=data)
summary(lm.p)
# y = -2.06* ( -0.502x1 -0.5x2 -0.498x3 -0.501x4)

#9.3
x<- c(1.000, 0.846, 0.805, 0.859, 0.473, 0.398, 0.301, 0.382,
      0.846, 1.000, 0.881, 0.826, 0.376, 0.326, 0.277, 0.277, 
      0.805, 0.881, 1.000, 0.801, 0.380, 0.319, 0.237, 0.345, 
      0.859, 0.826, 0.801, 1.000, 0.436, 0.329, 0.327, 0.365, 
      0.473, 0.376, 0.380, 0.436, 1.000, 0.762, 0.730, 0.629, 
      0.398, 0.326, 0.319, 0.329, 0.762, 1.000, 0.583, 0.577, 
      0.301, 0.277, 0.237, 0.327, 0.730, 0.583, 1.000, 0.539, 
      0.382, 0.415, 0.345, 0.365, 0.629, 0.577, 0.539, 1.000)
names<-c("身高 x1", "手臂长 x2", "上肢长 x3", "下肢长 x4", "体重 x5", 
         "颈围 x6", "胸围 x7", "胸宽 x8")
r<-matrix(x, nrow=8, dimnames=list(names, names))
(FA1<-factanal(covmat= r,factors=2,rot="varimax"))

#第一因子：身高，臂长——长度因子
#第二因子：体重，胸围，胸宽——宽度因子 哈哈！


#9.4（1）

x <- read.table("clipboard", header=T)
names(x) <- c("政治","语文","外语","数学","物理")
factanal(x,factors=2,rot="varimax")

#第一因子为文科，政治语文外语系数较大
#第二因子为理科，数学物理系数叫啊



