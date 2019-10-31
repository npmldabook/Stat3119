
library(plotrix)
#plot region to set coordinates
plot( c(0,10), c(0,10), type="n", main="Happy Halloween!", col.main=2, axes=F, xlab="", ylab="",cex.main=1.5)

# stem
polygon(x=c(4.1,4.33, 4.4, 5.5, 5.75,  5.9), y=c(7.7,8, 9.9, 9.9, 8,  7.7)+0.4, col='dark green')
polygon(x=c(4.1,4.33, 4.4, 5.5, 5.75,  5.9),
        y=c(7.7,8, 9.9, 9.9, 8,  7.7)+0.4,  col='darkgoldenrod4', border = F, density=5, lwd=3, angle=95)

# face
draw.ellipse( 5 , 4.1, a=4.3, b=4.1, col='dark orange' , angle=0)

# eyes
draw.ellipse(c(7,3), c( 5,5),  c(1,1),  c(0.8, 0.8), col=1,    angle=0, border = F)

draw.ellipse(7, 5, 1, 0.80, col='dark orange', angle=0, segment=c(0,50), arc.only=FALSE, border = F)
draw.ellipse(3, 5, 1, 0.80, col='dark orange', angle=0, segment=c(0,50), arc.only=FALSE, border = F)

# nose
polygon(x=c(4.45, 5, 5.55, 4.7), y=c(2.8, 4.2, 2.8, 2.8), col=1, border = F)

# teeth
polygon( x=c(3.7, 2.5, 3.9, 3.9, 4.3, 4.3,5.6, 5.6, 6, 6, 7.5, 6.3, 5.2, 5.2, 4.7, 4.7, 3.7), 
         y=c(0.8, 2.2,1.85, 1.5,1.5,1.85, 1.85, 1.5,1.5,1.85,2.2, 0.8, 0.75, 1.1, 1.1,0.75 ,0.8), 
         col=1, border = F)

# add date
 mtext(Sys.Date(), font=3, col='brown', cex=1.8, side=1, line=0.5)