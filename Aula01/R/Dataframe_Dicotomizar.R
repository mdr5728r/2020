# Dataframe_Dicotomizar.R

library(openxlsx)
library(readxl)
Dados <- read_excel(file.path("dados","Adm2008_v3.xlsx"))
View(Dados)
classe <- c("Anormal",
            "Normal",
            "Anormal")
pc <- c(0, 18.5, 24.9, +Inf)
Dados$IMC_dicot <- cut(Dados$IMC, pc, classe)
View(Dados)
write.xlsx(Dados, file.path("dados","Adm2008_v4.xlsx")) 

# grafico pie (opcional)
dt_col <- table(Dados$IMC_dicot)
dt_col <- dt_col[dt_col>0] # elimina classes com contagem == 0
nomes <- names(dt_col)
fatias <- as.numeric(dt_col)
pie(fatias, labels=nomes, main="Categorias pelo IMC")