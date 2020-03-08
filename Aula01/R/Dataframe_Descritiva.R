# Dataframe_Descritiva.R
# importa planilha Excel e
# mostra uma visao geral dos dados
# Adm2008_v2.xlsx foi criada com Dataframe_ExcluirVariavel.R

library(readxl)
Dados <- readxl::read_excel(file.path("dados", "Adm2008_v2.xlsx"))

colunas <- names(Dados)
cat ("\nnomes das colunas:\n")
print (colunas)

cat ("\nestrutura do dataframe:\n")
str(Dados)

cat ("\nvisao geral dos dados:\n")
sumario <- summary(Dados)
print (sumario)

