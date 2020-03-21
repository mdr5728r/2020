# Exemplo de conversao de tabela de dados em 
# uma tabela de contingencia
# Uma tabela de dados costuma ser apresentada
# com um paciente por linha, e os dados em colunas

library("epiR")

filename <- "MPT0164_GESTANTES_AFolico_B12_rubeola.txt"
gestantes <- read.table(filename,header=TRUE,sep="\t",dec=".")

# adicionando duas colunas com categorias
gestantes$HBcat[gestantes$HB < 12] <- "Gold+"
gestantes$HBcat[gestantes$HB >= 12] <- "Gold-"
gestantes$HTcat[gestantes$HT < 37] <- "Anemia+"
gestantes$HTcat[gestantes$HT >= 37] <- "Anemia-"

# transformando em tabela de contingencia 2x2
tabela <- table (gestantes$HTcat, gestantes$HBcat)
# print (tabela)

# aplicacao da regra de Bayes
cat ("\nAplicando a regra de Bayes:\n")
out <- epi.tests(tabela, conf.level = 0.95)
print (out)
sumario <- summary(out)
print (sumario)

