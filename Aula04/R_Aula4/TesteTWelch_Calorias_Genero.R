library(readxl)
library(MKinfer)
Dados <- read_excel("~/FMUSP/Sameshima/MDR5728 - Bioestatística em R para Métodos Diagnósticos em Medicina/Aula04/Dados/ConsumoCaloricoDiario&Genero.xlsx")
t.test(Calorias ~ factor(Genero), data=Dados, var.equal=TRUE)
# MKinfer::boot.t.test realiza o teste t de Welch tambem
MKinfer::boot.t.test(Calorias ~ factor(Genero), data=Dados, R=10^6) 


