library(haven)
Dados <- haven::read_sav("Simpatia de enfermeiros.sav")
with(Dados, wilcox.test(Depois, Antes, mu=0, 
                        paired = TRUE, alternative = "greater",
                        conf.int=TRUE))
medianDepois <- median(Dados$Depois)
medianAntes <- median(Dados$Antes)
dif <- medianDepois - medianAntes
cat("Diferenca das medianas (Depois-Antes) = ",dif,sep="")
     