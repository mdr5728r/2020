library(coin)
library(epiR)
library(DescTools)
library(psych)
# Siegel & Castellan (1988), p. 77
Tabela <- ("
 TVDebate Carter Reagan
  Carter  28     13
  Reagan  7      27
")
print(TC <- as.matrix(read.table(textConnection(Tabela), 
                                 header=TRUE, row.names=1)))
print(mcnemar.test(TC,correct=FALSE)) # classico
# McNemar's Chi-squared test
# data:  TC
# McNemar's chi-squared = 1.8, df = 1, p-value = 0.1797
print(coin::mh_test(as.table(TC), distribution = "exact")) # robusto
# Exact Marginal Homogeneity Test
# data:  response by
# conditions (Var1, Var2) 
# stratified by block
# chi-squared = 1.8, p-value = 0.2632

# Agresti (1990), apud Mehta, C. R. &  
# Patel, N. R. (1996) SPSS Exact Tests 7 for  Windows. IL: SPSS, p. 72.
# Pap-Smear Classification by Two Pathologists
# O objetivo é analisar a concordância de diagnóstico entre 2 patologistas 
# que  classificaram conforme a sereridade de uma determinada 
# lesão uterina de 118 slides de diferentes mulheres.
# N=Negativo, HEA=Hiperplasia escamosa atipica, CIS=carcinoma in situ
# CE=Carcinoma escamoso, CI=Carcinoma invasivo
Tabela <- ("
 Patologistas N  HEA CIS CE CI
          N   22 2   2   0  0
          HEA 5  7   14  0  0
          CIS 0  2   36  0  0
          CE  0  1   14  7  0
          CI  0  0   3   0  3
")
print(TC <- as.matrix(read.table(textConnection(Tabela), 
                                 header=TRUE, row.names=1)))
cat("\nTeste de concordancia: delineamento intraparticipantes\n")
# Exact Marginal Homogeneity Test for Ordered Data
print(coin::mh_test(as.table(TC), distribution = "exact",
                    scores = list(response = 1:nrow(TC)))) # robusto
# data:  response (ordered) by
# conditions (Var1, Var2) 
# stratified by block
# Z = 1.1523, p-value = 0.3073
# alternative hypothesis: two.sided
cat("Teste kappa de Cohen\n")
cat("H0: kappa = 0 vs H1: kappa != 0\n",sep="")
print(res <- psych::cohen.kappa(TC))
# Cohen Kappa and Weighted Kappa correlation coefficients and 
# confidence boundaries 
#                  lower estimate upper
# unweighted kappa  0.39     0.50  0.61
# weighted kappa    0.78     0.78  0.78

k <- res$kappa
if (0 <= k & k < 0.1) {gkl <- "Poor"}
if (0.1 <= k & k < 0.2) {gkl <- "Slight"}
if (0.2 <= k & k < 0.4) {gkl <- "Fair"}
if (0.4 <= k & k < 0.6) {gkl <- "Moderate"}
if (0.6 <= k & k < 0.8) {gkl <- "Substancial"}
if (0.8 <= k & k <= 1.0) {gkl <- "Almost perfect"}
cat("Grau de concordancia entre dois metodos/avaliadores = ",gkl,"\n",sep="")


