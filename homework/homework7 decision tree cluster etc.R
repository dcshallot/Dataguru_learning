


library(rpart)

data(kyphosis)

tree <- rpart(Kyphosis~.,data=kyphosis,method='anova')
#绘制决策时模型图
plot(tree,uniform=T,branch=0,margin=0.1,main="kyphosis Classification Tree")
text(tree,use.n=T,fancy=T,col='blue')  #不知道为啥，图会错位

# 8.3
edu=data.frame(
  DXBZ=c( 9.3, 4.67, 0.96, 1.38, 1.48, 2.6, 2.15, 2.14, 6.53, 1.47, 
          1.17, 0.88, 1.23, 0.99, 0.98, 0.85, 1.57, 1.14, 1.34, 0.79,
          1.24, 0.96, 0.78, 0.81, 0.57, 1.67, 1.1, 1.49, 1.61, 1.85), 
  CZBZ=c( 30.55, 29.38, 24.69, 29.24, 25.47, 32.32, 26.31, 28.46, 31.59, 
          26.43, 23.74, 19.97, 16.87, 18.84, 25.18, 26.55, 23.16, 22.57,
          23.04, 19.14, 22.53, 21.65, 14.65, 13.85, 3.85, 24.36, 16.85, 17.76, 20.27,20.66), 
  WMBZ=c (8.7,8.92, 15.21, 11.3, 15.39, 8.81, 10.49, 10.87, 11.04 ,17.23,
          17.46, 24.43, 15.63, 16.22, 16.87, 16.15, 15.79 , 12.1, 10.45, 
          10.61, 13.97, 16.24, 24.27, 25.44, 44.43, 17.62, 27.93, 27.7, 22.06, 12.75) )
rownames(edu)=c('北京','天津','河北','山西','内蒙古','辽宁','吉林','黑龙江',
                '上海','江苏','浙江','安徽','福建','江西','山东','河南','湖北',
                '湖南','广东','广西','海南','四川','贵州','云南','西藏','陕西',
                '甘肃','青海','宁夏','新疆')

d <- dist ( edu, method='euclidean')
hc1 <- hclust(d, 'complete')
hc2 <- hclust(d, 'average')
hc3 <- hclust(d, 'centroid')
hc4 <- hclust(d, 'ward')

plot(hc1, hang=-1)
rect.hclust(hc1, k=4)
plot(hc2, hang=-1)
rect.hclust(hc2, k=4)
plot(hc3, hang=-1)
rect.hclust(hc3, k=4)
plot(hc4, hang=-1)
rect.hclust(hc4, k=4)

# by kmeans
km <- kmeans(edu,4)
t <- km$cluster
t[t==1]
t[t==2]
t[t==3]
t[t==4]


# 8.4
app <- read.table("applicant.data")
d <- 1-cor(app)
d <- as.dist(d)

hc1 = hclust(d, 'complete')
hc2 = hclust(d, 'average')
hc3 = hclust(d, 'centroid')
hc4 = hclust(d, 'ward')

plot(hc1, hang=-1)
rect.hclust(hc1, k=5)
plot(hc2, hang=-1)
rect.hclust(hc2, k=5)
plot(hc3, hang=-1)
rect.hclust(hc3, k=5)
plot(hc4, hang=-1)
rect.hclust(hc4, k=5) 



# heat plot
setwd("D:/github/2014_Dataguru_learning_git/homework")
who <- read.csv("WHO.csv")

count <- who[ c(5,37,109,131,150,175,177,180,189,193),
              c(5,6,9,10,11,12,13,18,21,23) ] 
rownames(count) <- who[c(5,37,109,131,150,175,177,180,189,193) ,1]
data_matrix <- data.matrix( count)
data_matrix <- scale( data_matrix, scale=T, center=T)
summary(data_matrix)

pal <- heat.colors(10)
breaks <- seq(-1,1,0.2)

par(mar = c(3,7,7,2),oma=c(0.2,0.2,0.2,0.2),mex=0.5)
image(x=1:nrow(data_matrix),y=1:ncol(data_matrix),
      z=data_matrix,xlab="",ylab="",breaks=breaks,
      col=pal,axes=FALSE)

text(x=1:nrow(data_matrix)+0.75, y=par("usr")[4] + 1.25,
     srt = 30, adj = 1, labels = rownames(data_matrix),
     xpd = TRUE)


names(who)[c(5,6,9,10,11,12,13,18,21,23)]
colnames(data_matrix) <- c("adult literacy", "income","population","pop growth",
                           "urban pop", "poverty","median age","antenatal care",
                           "health brith","child itamin")
axis(2,at=1:ncol(data_matrix),labels=colnames(data_matrix),
     col="white",las=2)
abline(h=c(1:ncol(data_matrix))+0.5,v=c(1:nrow(data_matrix))+0.5,
       col="white",lwd=2,xpd=F)
title("Correlation between genes",line=8,adj=0)
box()


