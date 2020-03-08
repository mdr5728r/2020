# Dataframe_Categorizar.R

library(openxlsx)
library(readxl)
Dados <- read_excel(
  file.path("dados","Adm2008_v2.xlsx")
)
View(Dados)
Dados$IMC <- Dados$MCT/(Dados$Estatura^2)
classe <- c("Abaixo do peso",
            "Normal", "Sobprepeso", 
            "Obesidade I", "Obesidade II", "Obesidade III")
pc <- c(0, 18.5, 24.9, 29.9, 34.9, 39.9, +Inf)
Dados$IMC_classe <- cut(Dados$IMC, pc, classe)
View(Dados)
write.xlsx (Dados,
            file.path("dados","Adm2008_v3.xlsx")
) 

# grafico pie (opcional)
dt_col <- table(Dados$IMC_classe)
dt_col <- dt_col[dt_col>0] # elimina classes com contagem == 0
nomes <- names(dt_col)
fatias <- as.numeric(dt_col)
pie(fatias, labels=nomes, main="Categorias pelo IMC")
