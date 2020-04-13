# demo_r_de_Spearman_boot.R

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
bg <- eiras.friendlycolor(24) # amarelo
pch <- 21 # circulo

# valores
x <- 2:10
y <- c(1, 2.5, 2.4, 11, 5.8, 13.8, 14, 8.3, 13)
# para nao ter um ponto na coordenada do centroide
# (melhora o exemplo)
centro_x <- mean(x)
x[x==centro_x] <- x[x==centro_x]-0.5
cat("x:",x,"\n")
cat("y:",y,"\n")
eiras.cor.test.boot (x, y,
                     alpha=alfa, B=B, 
                     method="spearman",
                     xlab="x", ylab="y", 
                     col=col, bg=bg, pch=pch)

# enable warnings
options(warn=0)
