library(readxl)
library(MKinfer)
Dados <- read_excel("~/FMUSP/Sameshima/MDR5728 - Bioestatística em R para Métodos Diagnósticos em Medicina/Aula04/Dados/DiametroAbdominal&CremeRedutor.xlsx")
diferenca <- Dados$Depois - Dados$Antes
MKinfer::boot.t.test(diferenca, mu=0, R=1e6)
d <- density(diferenca)
plot(d)
