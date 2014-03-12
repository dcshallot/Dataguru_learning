
#2.2
A <- matrix( seq(1:20) , nrow=4, ncol=5)
B <- matrix( seq(1:20) , nrow=4, ncol=5, byrow = T)
c <- A+B
D <- A%*%t(B)  ##
E <- A*B
Ff <- A[1:3, 1:3]
G <- B[, -3]

#2.3
x <- c( rep(1,5), rep(2,3) , rep(3,4) , rep(4,2))

#2.4
H <- matrix( nrow=5, ncol=5)
for (i in 1:5) {
  for (j in 1:5) {
    H[i,j] = 1/( i+j -1)
    
  }
}

det(H)
solve(H)
eigen(H)

#2.5
students <- data.frame( id = seq(1:5) , name=c("张三","李四","王五","赵六","丁一"),
                        gender = c("女","男","女","男","女") , 
                        age = c(14,15,16,14,15), 
                        height = c(156,165,157,162,159),
                        weight = c(42.0, 49.0, 41.5, 52.0, 45.5))

#2.6
data <- read.table(file = "data.txt",header = TRUE)
write.csv(data, "data.csv", row.names = FALSE)

#3.5
test.live <- data.frame( type = c( rep(1,11), rep(2,10), rep(3,12)) , 
                         days = c( 2,4,3,2,4,7,7,2,2,5,4,5,6,8,5,10,7,
                                   12,12,6,6,7,11,6,6,7,9,5,5,10,6,3,10) )
boxplot( test.live$ days~ test.live$type)
plot( as.factor(test.live$type ) , test.live$ days )
#从图看来，不同类别的平均存活天数上，1类与2,3类差异较大

#3.7
student <- data.frame( gender = c(rep("f",9), rep("m",10)) ,
              age = c(13,13,14,12,12,15,11,15,14,14,14,15,12,13,12,16,12,11,15),
              height =c(56.5, 65.3, 64.3, 56.3, 59.8, 66.5, 51.3, 62.5, 62.8, 
                        69, 63.5, 67, 57.3, 62.5, 59, 72, 64.8, 57.5, 66.5),
              weight =c(84,98,90,77,84,112,50.5,112.5,102.5,112.5,102.5,133,83,84, 99.5,150,128,85,112)
)
plot( student$height, student$weight)
coplot( student$height~ student$weight | student$gender)
coplot( student$height~ student$weight | student$age)
coplot( student$height~ student$weight | student$age + student$gender)

#3.8
x <- seq( -2 ,3 , 0.05)
y <- seq( -1, 7, 0.05)
z <- function(x ,y) x^4 -2*x^2*y +x^2 -2*x*y +2*y^2 +9/2*x -4*y+4
o <- outer(x, y, z)
contour(x,y,o, col ="blue", levels=c(0,1,2,3,4,5,10,15,20,30,40,50,60,80,100))
persp(x,y,o, theta=90, phi=20, expand = 0.6, col ='green')

#3.10
applicant <- read.csv("~/applicant.csv")
stars(applicant)
g.applicant <- transform( applicant, g1 =(SC+LC+SMS+DRV+AMB+GSP+ POT) /7 ,
                          g2 = (FL+EXP+SUIT)/3,
                          g3 = (LA+HON+KJ)/3,
                          g4 = AA,
                          g5 = APP)
stars(g.applicant[,17:21] )

#3.11
unison <- function(x){ #调和曲线图公式
  if (is.data.frame(x) == TRUE)
    x <- as.matrix(x)
  t <- seq(-pi, pi, pi/30)
  m <- nrow(x); n<-ncol(x)
  f <- array(0, c(m,length(t)))
  for(i in 1:m){
    f[i,] <- x[i,1]/sqrt(2)
    for( j in 2:n){
      if (j%%2 == 0)
        f[i,] <- f[i,]+x[i,j]*sin(j/2*t)
      else
        f[i,] <- f[i,]+x[i,j]*cos(j%/%2*t)
    }
  }
  plot(c(-pi,pi), c(min(f), max(f)), type = "n",
       main = "The Unison graph of Data",
       xlab = "t", ylab = "f(t)")
  for(i in 1:m) lines(t, f[i,] , col = i)
}

unison(g.applicant[,17:21])




