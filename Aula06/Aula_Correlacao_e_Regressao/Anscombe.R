# Anscombe.R

library(readxl)

source("eiras.friendlycolor.R")
source("eiras.cor.test.boot.R")

col <- eiras.friendlycolor(c(1,7,13,19))
pch <- 21:24

Anscombe <- read_excel("Anscombe.xlsx")

min.x <- min(c(Anscombe$x1,Anscombe$x2,Anscombe$x3,Anscombe$x4),na.rm=TRUE)
max.x <- max(c(Anscombe$x1,Anscombe$x2,Anscombe$x3,Anscombe$x4),na.rm=TRUE)
min.y <- min(c(Anscombe$y1,Anscombe$y2,Anscombe$y3,Anscombe$y4),na.rm=TRUE)
max.y <- max(c(Anscombe$y1,Anscombe$y2,Anscombe$y3,Anscombe$y4),na.rm=TRUE)

for (i in 1:4)
{
  if (i==1)
  {
    x <- Anscombe$x1
    y <- Anscombe$y1
  }
  if (i==2)
  {
    x <- Anscombe$x2
    y <- Anscombe$y2
  }
  if (i==3)
  {
    x <- Anscombe$x3
    y <- Anscombe$y3
  }
  if (i==4)
  {
    x <- Anscombe$x4
    y <- Anscombe$y4
  }
  
  eiras.cor.test.boot (x, y,
                       alpha=0.05, B=0, 
                       jitter=0,
                       main=paste("conjunto ",i,sep=""),
                       xlab="x", ylab="y",
                       xlim=list(min.x,max.x), 
                       ylim=list(min.y,max.y), 
                       bg=col[i], col="black", pch=pch[i])
}
