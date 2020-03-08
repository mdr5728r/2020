# Dataframe_ExcluirVariavel.R
# abre planilha Excel, remove colunas e
# salva planilha Excel com outro nome

library(openxlsx)
library(readxl)
Dados <- readxl::read_excel(file.path("dados", "Adm2008.xlsx"))
View(Dados)
Dados[,"Nome"]  <- NULL
Dados[,"Aprovado"]  <- NULL
Dados[,"Idade_Cat"]  <- NULL
View(Dados)
openxlsx::write.xlsx(Dados, file.path("dados", "Adm2008_v2.xlsx")) 
