# gestantes.R
#   le os dados e cria os graficos

source("friendlycolor.R")

# le os dados
DrgGrv <- read.table("Drogas_Gravidez.txt", header=TRUE, sep="\t")

# cria coluna para a porcentagem
DrgGrv$Porcentagem <- round(DrgGrv$Pacientes/sum(DrgGrv$Pacientes)*100,2)

# exibe o grafico no estilo 'histograma'
# (variavel quantitativa discreta)
plot (DrgGrv$Drogas, DrgGrv$Porcentagem,
      main = "Distribuição do uso de\ndrogas em gestantes",
      type="h", col=friendlycolor(20), lwd=3,
      xlab="Número de drogas utilizadas",
      ylab="Porcentagem de gestantes")
points(DrgGrv$Drogas, DrgGrv$Porcentagem,
       pch=21, 
       col=friendlycolor(20), bg=friendlycolor(23))

# tratando a variavel como quantitativa continua
# (apenas por exemplo; nao eh apropriado fazer isto)
# cria uma populacao com as quantidades 
# de drogas utilizadas pelas pacientes
populacao <- c() # cria um vetor vazio
for (r in 1:nrow(DrgGrv))
{
  # acumula no vetor o número de drogas
  populacao <- c(populacao, rep(DrgGrv$Drogas, times=DrgGrv$Pacientes))
}
# usa a funcao R que transforma em densidade de probabilidade
densidade <- density(populacao)
# exibe o grafico no estilo 'density plot'
plot (densidade,
      main = "Distribuição do uso de\ndrogas em gestantes",
      col=friendlycolor(20), lwd=3,
      xlab="Número de drogas utilizadas",
      ylab="Porcentagem de gestantes")
