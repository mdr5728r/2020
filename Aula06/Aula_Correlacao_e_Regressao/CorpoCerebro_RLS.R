# CorpoCerebro_RLS.R
# (versao nao padronizada, para regressão)

alfa <- 0.05

library(readxl)
library(dplyr)

source("eiras.jitter.R")
source("eiras.friendlycolor.R")
source("eiras.cor.test.boot.R")

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
                     bg=col_Cerebro, col="black", pch=pch_Cerebro)
