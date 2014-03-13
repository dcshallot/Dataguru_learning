

#8.1
x1 <- c(-1.9 , -6.9, 5.2, 5.0, 7.3, 6.8, 0.9, -12.5, 1.5, 3.8 ,
        0.2, -0.1, 0.4, 2.7, 2.1, -4.6, -1.7, -2.6, 2.6, -2.8)
x2 <- c(3.2, 10.4, 2.0, 2.5, 0.0, 12.7, -15.4, -2.5, 1.3, 6.8, 
        0.2, 7.5, 14.6, 8.3, 0.8, 4.3, 10.9, 13.1, 12.8, 10.0)
y <- factor(c( rep(2,10) , rep(1,10)) ) # 2=ring
x <- cbind(x1,x2) # trnx
t <- c(8.1, 2.0)
#距离判别
distinguish.distance( x,y, t, var.equal = FALSE )

#贝叶斯
distinguish.bayes( TrnX=x, TrnG=y, TstX=t, var.equal = FALSE)
distinguish.bayes( TrnX=x, TrnG=y, TstX=t, var.equal = T)

#Fisher
discriminiant.fisher( TrnX1 = cbind(x1,x2)[1:10,] , TrnX2 = cbind(x1,x2)[11:20,],  TstX = t)

#三种方法均判断要下雨

#8.2

diabetes <- read.table( "clipboard" , header=T)
#距离
df <- distinguish.distance(  trnx=diabetes[,-1] , trng=as.factor( diabetes[,1]) , var.equal = FALSE )
dt <- distinguish.distance(  trnx=diabetes[,-1] , trng=as.factor( diabetes[,1]) , var.equal = T )
#bayes
p <- c( rep(11/23,11 ), rep( 7/23 , 6), rep( 5/23 ,5))
bf <- distinguish.bayes( TrnX=diabetes[,-1], TrnG=as.factor( diabetes[,1]), p, var.equal = F)
bt <- distinguish.bayes( TrnX=diabetes[,-1], TrnG=as.factor( diabetes[,1]), p, var.equal = T)

table(diabetes[,1],df) #18
table(diabetes[,1],dt) #16
table(diabetes[,1],bf) #18
table(diabetes[,1],bt) #16

#考虑不同方差的算法效果好一些



