
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
  
  if ( var.equal == TRUE || var.equal ==T) { # same var
    s <- var(rbind(trnx1, trnx2))
    w <- mahalanobis( tstx, mu2, s) - mahalanobis( tstx, mu1, s) # test data distance with groups
  }
  else {                                # different var
    s1 <- var( trnx1) ; s2 <- var( trnx2)
    w <- mahalanobis( tstx, mu2, s2) - mahalanobis( tstx, mu1, s1) # test data distance with groups
  }
  
  for ( i in 1:nx) {
    if ( w[i] > 0) blong[i] <- 1
    else blong[i] <- 2 
  }
  blong
}

#multi-groups

distinguish.distance <- function
  ( trnx, trng, tstx = NULL, var.equal = F) { #trnx training, trng group factors
  
  if ( is.factor(trng) == F) {  # make this function works for above function, input as above can work
    mx <- nrow( trnx) ; mg <- nrow( trng)
    trnx <- rbind( trnx, trng )
    trng <- factor( rep(1:2, c(mx, mg)))
  }
  if (is.null( tstx) ==T ) tstx <- trnx
  if (is.vector(tstx) == T ) tstx <- t(as.matrix(tstx))
  else if ( is.matrix(tstx) != T ) tstx <- as.matrix(tstx)
  
  if( is.matrix(trnx) != T ) trnx <- as.matrix(trnx)
  
  nx <- nrow(tstx)
  blong <- matrix( rep( 0, nx), nrow =1, dimnames = list("blong" , 1:nx))
  g <- length( levels(trng))
  mu <- matrix( 0, nrow = g, ncol = ncol(trnx))
  
  for ( i in 1:g)
    mu[i,] <- colMeans( trnx[trng ==i,])
  D <- matrix( 0, nrow = g, ncol = nx)
  if (var.equal == T || var.equal ==TRUE) {
    for ( i in 1:g)
      D[i,] <- mahalanobis( tstx, mu[i,], var(trnx))
  }
  else{
    for ( i in 1:g)
      D[i,] <- mahalanobis( tstx, mu[i,], var(trnx[trng == i,]))
  }
  for ( j in 1:nx) {
    dmin <- Inf
    for ( i in 1:g)
      if (D[i,j] < dmin) {
        dmin <- D[i,j]; blong[j] <- i
      }
  }
  blong
}
