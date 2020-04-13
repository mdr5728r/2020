# Gestantes_rPearson.R

library(readxl)

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
                     alpha=0.05, B=0, 
                     z.score=TRUE,
                     xlab="Hematocrito (z)", ylab="Hemoglobina (z)", 
                     bg=col_HB, col="black", pch=pch_HB)

# HT x HEM
eiras.cor.test.boot (Gestantes$HT, Gestantes$HEM,
                     alpha=0.05, B=0, 
                     z.score=TRUE,
                     xlab="Hematocrito (z)", ylab="Hemoglobina (z)", 
                     bg=col_HEM, col="black", pch=pch_HEM)

# HT x LEUC
eiras.cor.test.boot (Gestantes$HT, Gestantes$LEUC,
                     alpha=0.05, B=0, 
                     z.score=TRUE,
                     xlab="Hematocrito (z)", ylab="Hemoglobina (z)", 
                     bg=col_LEUC, col="black", pch=pch_LEUC)

