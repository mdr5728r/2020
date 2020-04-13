# eiras.density_and_normal.R

source("eiras.friendlycolor.R")

eiras.density_and_normal <- function (valores, 
                                      col="black",
                                      main="",
                                      xlab="", ylab="")
{
  densidade <- density(valores)
  media <- mean(valores)
  desvpad <- sd(valores)
  x <- seq(media-3*desvpad, 
           media+3*desvpad, length.out=100)
  y <- dnorm(x, mean=media, sd=desvpad)
  plot(densidade,
       xlim=c(min(x),max(x)),
       ylim=c(0, max(y,densidade$y,na.rm=TRUE)),
       main=main,
       xlab=xlab,
       ylab=ylab,
       col=col,
       lty=1,
       lwd=3,
       type="l")
  lines(x,y,lty=2,lwd=2,col=col)
}

