# Criar uma tabela de contingencia
# a partir de sensibilidade, especificidade, 
# prevalencia e tamanho da amostra conhecidas

library("epiR")

sensibilidade <- 0.90
especificidade <- 0.80
prevalencia <- 0.01
# qualquer n razoavelmente grande pode ser usado, 
# ao escolhe-lo, afeta os intervalos de confianca
n <- 1

# recria a tabela
tabela <- as.table(matrix(n*c(prevalencia*sensibilidade,       (1 - prevalencia)*(1 - especificidade),
                              prevalencia*(1 - sensibilidade), (1 - prevalencia)*especificidade), 
                          nrow = 2, byrow = TRUE))
colnames(tabela) <- c("D+","D-")
rownames(tabela) <- c("T+","T-")

# aplicacao da regra de Bayes
cat ("\nAplicando a regra de Bayes:\n")

cat ("\ntabela\n")
out <- epi.tests(tabela, conf.level = 0.95)
print (out)
sumario <- summary(out)
print (sumario)

