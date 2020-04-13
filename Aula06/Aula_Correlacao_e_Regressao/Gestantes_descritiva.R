#Gestantes_descritiva.R

library(readxl)

source("ps_jitter.R")
source("friendlycolor.R")
col_HT <- friendlycolor(25) # marrom
pch_HT <- 21
col_HB <- friendlycolor(30) # tijolo
pch_HB <- 22
col_HEM <- friendlycolor(28) # bordo
pch_HEM <- 23
col_LEUC <- friendlycolor(9) # azul
pch_LEUC <- 24

Gestantes <- read_excel("Gestantes.xlsx")
grupos <- unique(Gestantes$Grupo)

# estatistica descritiva
cat("\nHematocrito:\n\tTotal:\n")
print(summary(Gestantes$HT))
cat("\n\tpor grupo:\n")
print(with(Gestantes, tapply(HT, Grupo, summary)))

cat("\nHemoglobina:\n\tTotal:\n")
print(summary(Gestantes$HB))
cat("\n\tpor grupo:\n")
print(with(Gestantes, tapply(HB, Grupo, summary)))

cat("\nHemacias:\n\tTotal:\n")
print(summary(Gestantes$HEM))
cat("\n\tpor grupo:\n")
print(with(Gestantes, tapply(HEM, Grupo, summary)))

cat("\nLeucocitos:\n\tTotal:\n")
print(summary(Gestantes$LEUC))
cat("\n\tpor grupo:\n")
print(with(Gestantes, tapply(LEUC, Grupo, summary)))

# gráficos
with(Gestantes, plot(ps_jitter(HT), ps_jitter(HB),
                     xlab="Hematócrito (%)", ylab="Memoglobina (mg/dl)",
                     col=col_HB, bg=col_HB, pch=pch_HB))
with(Gestantes, plot(ps_jitter(HT), ps_jitter(HEM), 
                     xlab="Hematócrito (%)", ylab="Hemácias (milhões/mm³)",
                     col=col_HEM, bg=col_HEM, pch=pch_HEM))
with(Gestantes, plot(ps_jitter(HT), ps_jitter(LEUC), 
                     xlab="Hematócrito (%)", ylab="Leucócitos (milhares/mm³)",
                     col=col_LEUC, bg=col_LEUC, pch=pch_LEUC))
