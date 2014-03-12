
par(bg="grey25");#±³¾°
col1 <- "#f05705"
col2 <- "#32b7fa"
col3 <- "#7aeb2f"
plot(x=c(1),y=c(2.5),xlim=c(0,5),col="gray27",ylim=c(0,10),axes=F,ann=F,pch=20);#×ø±ê
#»­Ïß
lines(c(1,1),c(2.5,8),col=col1,lwd=5);
lines(c(2,2),c(2.5,8),col=col1,lwd=5);
lines(c(2,2),c(0,2.5),col=col2,lwd=5);
lines(c(3,3),c(2,4.7),col=col2,lwd=5);
lines(c(3,3),c(4.7,6),col=col3,lwd=5);
lines(c(3,3),c(6,8),col=col1,lwd=5);
lines(c(4,4),c(1,3.5),col=col2,lwd=5);
lines(c(4,4),c(3.5,4.8),col=col3,lwd=5);
lines(c(4,4),c(4.8,6.2),col=col1,lwd=5);
lines(c(4,4),c(6.2,7.2),col=col2,lwd=5);
lines(c(4,4),c(7.2,8),col=col1,lwd=5);
#ÎÄ×Ö
text(c(1:4),c(8.5),labels=c(paste("version ",1:4,sep="")),col="white");
text(0,c(1.0,0.5,0),labels=c("mary","suzanne","martin"),col=c(col1,col2,col3),cex=1.5,adj=0);



par(bg="grey25");#±³¾°
plot(x=c(1),y=c(2.5),xlim=c(0,5),col="gray27",ylim=c(0,10),axes=F,ann=F,pch=20);#×ø±ê
#»­Ïß
lines(c(1,1),c(2.5,8),col=col1,lwd=5);
lines(c(2,2),c(2.5,8),col=col1,lwd=5);
lines(c(2,2),c(0,2.5),col=col2,lwd=5);
lines(c(3,3),c(2,4.7),col=col2,lwd=5);
lines(c(3,3),c(4.7,6),col=col3,lwd=5);
lines(c(3,3),c(6,8),col=col1,lwd=5);
lines(c(4,4),c(1,3.5),col=col2,lwd=5);
lines(c(4,4),c(3.5,4.8),col=col3,lwd=5);
lines(c(4,4),c(4.8,6.2),col=col1,lwd=5);
lines(c(4,4),c(6.2,7.2),col=col2,lwd=5);
lines(c(4,4),c(7.2,8),col=col1,lwd=5);
#»­¾ØÕó
polygon(c(1,2,2,1),c(2.5,2.5,8,8),col=col1,border=col1,density=c(100));
polygon(c(2,3,3,2),c(6,6,8,8),col=col1,border=col1,density=c(100));
polygon(c(3,4,4,3),c(6,4.8,6.2,7.2),col=col1,border=col1,density=c(100));
polygon(c(3,4,4,3),c(7.2,7.2,8,8),col=col1,border=col1,density=c(100));
polygon(c(2,3,3,2),c(0,2,4.7,2.5),col=col1,border=col1,density=c(100));
polygon(c(3,4,4,3),c(2,1,3.5,4.7),col=col2,border=col2,density=c(100));
polygon(c(3,4,4,3),c(2,1,3.5,4.7),col=col2,border=col2,density=c(100));
polygon(c(3,4,4,3),c(4.7,3.5,4.8,6),col=col3,border=col3,density=c(100));

#ÎÄ×Ö
text(c(1:4),c(8.5),labels=c(paste("version",1:4,sep="")),col="white");
text(0,c(1.0,0.5,0),labels=c("mary","suzanne","martin"),col=c(col1,col2,col3));

