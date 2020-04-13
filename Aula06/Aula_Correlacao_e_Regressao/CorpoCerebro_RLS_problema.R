# CorpoCerebro_RLS_problema.R
# (versao nao padronizada, para regressão)

alfa <- 0.05

library(readxl)
library(dplyr)

source("eiras.jitter.R")
source("eiras.friendlycolor.R")
source("eiras.cor.test.boot.R")
source("eiras.density_and_normal.R")
col_Corpo <- eiras.friendlycolor(14) # verde
pch_Corpo <- 22
col_Cerebro <- eiras.friendlycolor(39) # cinza
pch_Cerebro <- 21

CorpoCerebro <- read_excel("CorpoCerebro.xlsx")

# HT x HB
eiras.cor.test.boot (CorpoCerebro$Corpo, CorpoCerebro$Cerebro,
                     alpha=alfa, B=0, 
                     method="lm",
                     jitter=0,
                     xlab="Corpo (kg)", ylab="Cerebro (g)", 
                     bg=col_Cerebro, col="black", pch=pch_Cerebro,
                     suppress.text=TRUE)
# marca os problemas
points(200,800,cex=10,lwd=4)
lines(c(0,7000),c(1500,11000),lwd=4,lty=5)
lines(c(300,7000),c(-50,5300),lwd=4,lty=5)

# density plots
eiras.density_and_normal(CorpoCerebro$Corpo, 
                         col=col_Corpo,
                         xlab="Corpo (kg)", ylab="densidade")
eiras.density_and_normal(CorpoCerebro$Cerebro, 
                         col=col_Cerebro,
                         xlab="Cerebro (g)", ylab="densidade")

