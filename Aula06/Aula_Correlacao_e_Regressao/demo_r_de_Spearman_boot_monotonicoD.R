# demo_r_de_Spearman_boot_monotonicoD.R

# disable warnings
options(warn=-1)

alfa <- 0.05
B <- 1e4

library(readxl)
library(dplyr)

source("eiras.jitter.R")
source("eiras.friendlycolor.R")
source("eiras.cor.test.boot.R")

# cores
col <-  eiras.friendlycolor(31) # preto
bg <- eiras.friendlycolor(4) # violeta
pch <- 21 # circulo

# valores
set.seed(27)
x <- 1:20
y <- c(runif(1,1600,1700))
for (i in 2:length(x))
{
  y <- c(y,runif(1,y[i-1]-i^2,y[i-1]+0.01))
}
cat("x:",x,"\n")
cat("y:",round(y,3),"\n")
eiras.cor.test.boot (x, y,
                     alpha=alfa, B=B, 
                     method="spearman",
                     jitter=0,
                     xlab="x", ylab="y", 
                     col=col, bg=bg, pch=pch)
lines(x,y,type="b")

# enable warnings
options(warn=0)
