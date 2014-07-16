
# 1.1
mtcars
library(plyr)

dlply( mtcars, "am", summarize, 
       mpg=mean(mpg), cyl= mean(cyl), disp=mean(disp), hp=mean(hp),
       drat=mean(drat), wt=mean(wt), qsec=mean(qsec), vs=mean(vs),
       gear=mean(gear), carb=mean(carb))

daply( mtcars, "am", summarize, 
       mpg=mean(mpg), cyl= mean(cyl), disp=mean(disp), hp=mean(hp),
       drat=mean(drat), wt=mean(wt), qsec=mean(qsec), vs=mean(vs),
       gear=mean(gear), carb=mean(carb))

ddply( mtcars, "am", summarize, 
       mpg=mean(mpg), cyl= mean(cyl), disp=mean(disp), hp=mean(hp),
       drat=mean(drat), wt=mean(wt), qsec=mean(qsec), vs=mean(vs),
       gear=mean(gear), carb=mean(carb))

# 1.2
le <- cut( mtcars$wt, c(0,2,4, max(mtcars$wt)) )
mt <- cbind(mtcars, le=as.numeric(le))
dlply( mt, "le", summarize, mpg=mean(mpg))
daply( mt, "le", summarize, mpg=mean(mpg))
ddply( mt, .(le), summarize, mpg=mean(mpg))

# 1.3
ddply( mt, .(le, am), summarize, mean(mpg))
ddply( mt, c("le","am" ), summarize, mean(mpg))


# 2.1
dc <- as.matrix( iris[,-5] )
alply( dc, 2, mean)
adply( dc, 2, mean)
aaply( dc, 2, mean)

alply( dc, 1, mean)
adply( dc, 1, mean)
aaply( dc, 1, mean)
