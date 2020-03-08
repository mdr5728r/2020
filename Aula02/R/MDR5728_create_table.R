# Exemplo de como montar uma tabela de contingencia
# quando temos os dados resumo

library("epiR")

#      D+   D-
# T+   a    b
# T-   c    d
a <- 731
b <- 270
c <- 78
d <- 1500

# cria a tabela
tabela <- as.table( matrix ( c(a, b, c, d), nrow = 2, byrow = TRUE))
colnames(tabela) <- c("Anemia+","Anemia-")
rownames(tabela) <- c("Ferritina+","Ferritina-")
 print (tabela) 

# aplicacao da regra de Bayes
cat ("\nAplicando a regra de Bayes:\n")

out <- epi.tests(tabela, conf.level = 0.95)
print (out)
sumario <- summary(out)
print (sumario)
