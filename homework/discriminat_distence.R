
#距离判别法函数自建
#Mahalanobis distance of groups in different var

#2 groups
discriminiant.distance <- function  # TRaiNing set x1,x2, TeSTx
( trnx1, trnx2, tstx = NULL, var.equal= FALSE) {
  if ( is.null(tstx) == T) tstx <- rbind( trnx1, trnx2 ) #if testset not given
  
  if ( is.vector(tstx) == T) tstx <- t(as.matrix(tstx))
  else if ( is.matrix(tstx) != T)  tstx <- as.matrix(tstx) #what ever tstx is, turn into matrix
  
  if ( is.matrix(trnx1) != T) trnx1 <- as.matrix(trnx1)
  if ( is.matrix(trnx2) != T) trnx1 <- as.matrix(trnx2)

  nx <- nrow(tstx) 
  blong <- matrix( rep( 0, nx) , nrow=1, byrow=T, 
                   dimnames=list("blong", 1:nx)) #used for result store
  
  mu1 <- colMeans(trnx1); mu2 <- colMeans(trnx2)
  
  if ( var.equal ==T|| var.equal ==T) { # same var
    s <- var(rbind(trnx1, trnx2))
    w <- mahalanobis( tstx, mu2, s) - mahalanobis( tstx, mu1, s)
  }
  else {                                # different var
    s1 <- var( trnx1) ; s2 <- var( trnx2)
    w <- mahalanobis( tstx, mu2, s2) - mahalanobis( tstx, mu1, s1)
  }
  
  for ( i in 1:nx) {
    if ( w[i] > 0) blong[i] <- 1
    else blong[i] <- 2 
  }
  blong
}


