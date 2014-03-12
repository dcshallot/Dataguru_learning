
#3.1
data <- c("
74.3 78.8 68.8 78.0 70.4 80.5 80.5 69.7 71.2 73.5
79.5 75.6 75.0 78.8 72.0 72.0 72.0 74.3 71.2 72.0
75.0 73.5 78.8 74.3 75.8 65.0 74.3 71.2 69.7 68.0
73.5 75.0 72.0 64.3 75.8 80.3 69.7 74.3 73.5 73.5
75.8 75.8 68.8 76.5 70.4 71.2 81.2 75.0 70.4 68.0
70.4 72.0 76.5 74.3 76.5 77.6 67.3 72.0 75.0 74.3
73.5 79.5 73.5 74.7 65.0 76.5 81.6 75.4 72.7 72.7
67.2 76.5 72.7 70.4 77.2 68.8 67.3 67.3 67.3 72.7
75.8 73.5 75.0 73.5 73.5 73.5 72.7 81.6 70.3 74.3
73.5 79.5 70.4 76.5 72.7 77.2 84.3 75.0 76.5 70.4
")
data <- gsub("\n", " ", data, fix=T)
data <- strsplit( data, " " )[[1]]
data <- as.numeric( data[ data!= ""] )

mean(data) #均值
var(data) #方差
sd(data) #标准差
range(data)[2] - range(data)[1] #极差
100*sd(data)/mean(data) #变异系数
length(data) /((length(data)-1)*(length(data) -2))*sum(( data -mean(data))^3 )/sd(data)^ 3 #峰度
(( length(data)*(length(data) +1)) /((length(data)-1)*(length(data)-2)*(length(data)- 3))
       * sum ((data- mean(data))^4 )/sd(data)^ 4 ) #偏度


#3.2
hist(data) #直方图
#密度估计曲线
hist(data , freq = FALSE )
lines(density(data), col = "blue")
x <- min(data) : max(data)
lines(x, dnorm(x, mean(data), sd(data)), col = "red")
#经验分布
plot(ecdf(data),verticals = TRUE, do.p = FALSE)
lines(x, pnorm(x, mean(data), sd(data)), col = "red")
#QQ图
qqnorm(data); qqline(data, col = "red")

#3.3
stem( data) #茎叶图
boxplot( data) #箱线图
fivenum( data, na.rm = TRUE) #五数总括

#3.4 W检验和K-S检验
shapiro.test( data )
ks.test(x, "pnorm", mean(data), sd(data) )

#3.9
student <- data.frame(
  Age=c(13, 13, 14, 12, 12, 15, 11, 15, 14, 14, 14, 15,
        12, 13, 12, 16, 12, 11, 15 ), 
  Height=c(56.5, 65.3, 64.3, 56.3, 59.8, 66.5, 51.3, 
           62.5, 62.8, 69.0, 63.5, 67.0, 57.3, 62.5, 
           59.0, 72.0, 64.8, 57.5, 66.5), 
  Weight=c( 84.0,  98.0,  90.0,  77.0,  84.5, 112.0, 
            50.5, 112.5, 102.5, 112.5, 102.5, 133.0,
            83.0,  84.0,  99.5, 150.0, 128.0,  85.0, 
            112.0)
); 
cor.test( student$Height , student$Weight) #显著相关

#6.1
x <- c(5.1,  7.8,	3.5,	4.5,	7.1,	5.6,	6.2,	8,	8.8,	6.4)
y <- c(1907,  3000,	1287,	1947,	2700,	2273,	2373,	3113,	3260,	2493)
plot( x, y)
m1 <- lm(y~x)
summary(m1) #显著
new <- data.frame(x = 7)
lm.pred <- predict( m1, new , interval="prediction", level=0.95); lm.pred

#6.2
#data imported from csv
data <- Book2
names(data) <- c("x1" , "x2" , "x3" , "y")
m2 <- lm( y ~ x1+x2+x3, data)
summary(m2)
m2.step <- step(m2)
summary(m2.step)
