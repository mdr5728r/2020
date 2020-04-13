# Gestantes_RLS.R
# (versao nao padronizada, para regressão)

alfa <- 0.05

library(readxl)
library(dplyr)

source("eiras.jitter.R")
source("eiras.friendlycolor.R")
source("eiras.cor.test.boot.R")

col_HB <- eiras.friendlycolor(30) # tijolo
pch_HB <- 22
col_HEM <- eiras.friendlycolor(28) # bordo
pch_HEM <- 23
col_LEUC <- eiras.friendlycolor(9) # azul
pch_LEUC <- 24

Gestantes <- read_excel("Gestantes.xlsx")

# HT x HB
eiras.cor.test.boot (Gestantes$HT, Gestantes$HB,
                     alpha=alfa, B=0, 
                     method="lm",
                     xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)", 
                     bg=col_HB, col="black", pch=pch_HB)
# HT x HEM
eiras.cor.test.boot (Gestantes$HT, Gestantes$HEM,
                     alpha=alfa, B=0, 
                     method="lm",
                     xlab="Hematocrito (%)", ylab="Hemacias (milhoes/mm³)", 
                     bg=col_HEM, col="black", pch=pch_HEM)
# HT x LEUC
eiras.cor.test.boot (Gestantes$HT, Gestantes$LEUC,
                     alpha=alfa, B=0, 
                     method="lm",
                     xlab="Hematocrito (%)", ylab="Leucocitos (milhares/mm³)", 
                     bg=col_LEUC, col="black", pch=pch_LEUC)
