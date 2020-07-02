eggs=1:200

Y=(eggs^2.2)/1e+02
par(mfrow=c(2,2)) 
plot(eggs, Y, type="l", lwd=4, las=1, xlab="Male Size", main="Larvae hatched over season", ylab="Number", xlim=c( 50, 200))
plot(eggs, Y/eggs, type="l", lwd=4, las=1, xlab="Male Size", ylab="", main="Index of care per egg", xlim=c(50, 200))