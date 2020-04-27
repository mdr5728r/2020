# TesteQuiQuadradoAderencia_Flores.R
library(readxl)

source ("eiras.quiaderencia.R")

# pega o arquivo e mostra seu conteúdo
df_flores <- read_excel("flores.xlsx")
cat("\nDados:\n")
prmatrix(df_flores, rowlab = rep("",nrow(df_flores)))

# chama os testes estatísticos
cat("\nHipotese de codominancia:\n")
eiras.quiaderencia(df_flores[,c(2,3)], B=1e6)

cat("\nHipotese de epistasis recessiva:\n")
eiras.quiaderencia(df_flores[,c(2,4)], B=1e6)
