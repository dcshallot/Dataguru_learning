
#社会网络图
a <- read.table("clipboard") #数据是个14*14的矩阵矩阵
b <- data.frame()
c <-1
for ( i in 1:nrow(a)) {
  for ( j in i:nrow(a)) {
     b[c,1] <- names(a)[i]
     b[c,2] <- row.names(a)[j]
     b[c,3] <- a[i,j]
     c <- c+1
  }
}
b <- b[which(b$V3 > 0) ,]
require(igraph)
g <- graph.data.frame(b, directed = F) 
plot(g,   
     edge.width=0.1 , #连线宽度
     edge.arrow.size=0.01,
     vertex.shape="square",
     vertex.size=10 ,#节点形状大小
     vertex.color="green" 
)

#书上内容
#6.3
x <- c( 1,1,1,1,2,2,2,3,3,3,4,4,4,5,6,6,6,7,7,7,8,8,8,9,11,12,12,12)
y <- c(.6,1.6,.5,1.2,2,1.3,2.5,2.2,2.4,1.2,3.5,4.1,5.1,5.7,3.4,9.7,
       8.6,4,5.5,10.5,17.5,13.4,4.5,30.4,12.4,13.4,26.2,7.4)
plot(x,y)
rg1 <- lm(y~x)
abline( rg1 )
summary(rg1) #T检验跟F检验均通过
y.res <- residuals(rg1);plot(y.res) #存在异方差
yy <- y^0.5
rg2 <- lm(yy ~x)
plot(x,yy)
abline(rg2)
summary(rg2)
yy.res <- residuals(rg2);plot(yy.res) 

#6.4
toothpaste<-data.frame(
  x1=c(-0.05, 0.25,0.60,0,   0.25,0.20, 0.15,0.05,-0.15, 0.15,
       0.20, 0.10,0.40,0.45,0.35,0.30, 0.50,0.50, 0.40,-0.05,
       -0.05,-0.10,0.20,0.10,0.50,0.60,-0.05,0,    0.05, 0.55),
  x2=c( 5.50,6.75,7.25,5.50,7.00,6.50,6.75,5.25,5.25,6.00,
        6.50,6.25,7.00,6.90,6.80,6.80,7.10,7.00,6.80,6.50,
        6.25,6.00,6.50,7.00,6.80,6.80,6.50,5.75,5.80,6.80),
  y =c( 7.38,8.51,9.52,7.50,9.33,8.28,8.75,7.87,7.10,8.00,
        7.89,8.15,9.10,8.86,8.90,8.87,9.26,9.00,8.75,7.95,
        7.65,7.27,8.00,8.50,8.75,9.21,8.27,7.67,7.93,9.26)
)
attach(toothpaste)
#样本自变量正态性
shapiro.test( toothpaste$x1)
shapiro.test( toothpaste$x2)  #非正态
shapiro.test( toothpaste$y)
plot(x1,x2)
cor(x1,x2) 
plot(x2,y)
detach(toothpaste)
#结论，x1,x2存在一定共线性，x2不从正态分布,不认为应该删除样本，而可能不应采用线性自变量形势.


#6.5
cement<-data.frame(
  X1=c( 7,  1, 11, 11,  7, 11,  3,  1,  2, 21,  1, 11, 10),
  X2=c(26, 29, 56, 31, 52, 55, 71, 31, 54, 47, 40, 66, 68),
  X3=c( 6, 15,  8,  8,  6,  9, 17, 22, 18,  4, 23,  9,  8),
  X4=c(60, 52, 20, 47, 33, 22,  6, 44, 22, 26, 34, 12, 12),
  Y =c(78.5, 74.3, 104.3,  87.6,  95.9, 109.2, 102.7, 72.5, 
       93.1,115.9,  83.8, 113.3, 109.4)
)

kappa( cor(cement[,1:4])) #>1000存在明显多重共线性
eigen( cor(cement[,1:4])) #可见X1-X4均存在共线问题，step不合理，应考虑抽取因子做回归

#6.6
y <- c(1,rep(0,17),rep(1,11),rep(0,89),rep(1,28),rep(0,30),rep(1,23), rep(0,3), rep(1,8),rep(0,32))
k <- c( rep(1,118), rep(0, 124))
j <- c( rep(1,18), rep(0,98), rep(1,60), rep(0,26),rep(1,40))
w <- c(rep(1,116), rep(0,2),rep(1,84),rep(0,40))

logi <- glm( y ~ k+j+w, family ="binomial" ) # y ~ x1+x2+x3
summary(logi) #结果表明，使用抗生素对控制感染的影响显著

#6.8 
cancer <- read.table("clipboard", header=T)
logi <- glm( y~ x1+x2+x3+x4+x5, cancer, family="binomial")
summary(logi) 
logi.s <- step( logi) 
summary(logi.s)
#只有x1生活能力，和x4肿瘤类型对生存时间有一定影响，其他变量影响不明显，逐步筛选的模型更好

