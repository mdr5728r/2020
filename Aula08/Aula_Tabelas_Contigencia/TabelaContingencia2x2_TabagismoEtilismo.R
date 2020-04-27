# TabelaContingencia2x2_TabagismoEtilismo.R

# desabilita warnings
options(warn=-1)

# Tabela de Dancey&Reidy (2019, p. 275)
TCtmp <- read_excel("tabagismo_e_etilismo_2x2.xlsx")
TC <- data.matrix(TCtmp)
rownames(TC) <-as.character(unlist(TCtmp[1:2,1]))
TC <- TC[,-1]
remove(TCtmp)
cat("\n------------------------------------------\n")
cat("Dados:")
cat("\n------------------------------------------\n")
print(TC)

cat("\n------------------------------------------\n")
cat("Analise de significancia estatistica:")
cat("\n------------------------------------------\n")
alfa <- 0.05

cat("\nTeste qui-quadrado de Pearson exato:\n")
print(res <- chisq.test(TC,simulate.p.value=TRUE,B=1e6))
cat("X^2 critico de 95% = ",qchisq(p=1-alfa,df=1), "\n", sep="")
N <- sum(res$observed)
nL <- nrow(TC) # ou dim(TC)[1]
nC <- ncol(TC) # ou dim(TC)[2]
df <- (nL-1)*(nC-1)
X2 <- res$statistic # estatistica de teste qui-quadrado
cat("Graus de liberdade (nao fornecidos por bootstrapping): ", df, "\n", sep="")
cat("Heuristica de significancia (rej. H0 se X^2/gl > 2): ", X2/df, "\n", sep="")

cat("\n------------------------------------------\n")
cat("Analise post hoc:")
cat("\n------------------------------------------\n")
cat("\nResiduos ajustados standardizados corrigidos por momento (MCSTARs):\n")
STAR <- res$stdres # STandardized Adjusted Residual (STAR)
MCSTAR <- STAR/(sqrt((1-1/nL)*(1-1/nC))) # Moment-correct (MCSTAR)
print(MCSTAR)
alfaBonf <- alfa/df
zcrit <- abs(qnorm(alfaBonf/2))
cat("\n|MCSTAR critico| (alfaBonferroni=5%/",df,") = ",zcrit,"\n", sep="")

cat("\n------------------------------------------\n")
cat("Analise de significancia pratica:")
cat("\n------------------------------------------\n")
phi2 <- X2/N # phi ou w de Cohen
Dim <- min(nL,nC)-1 # dimensoes da TC: dim = max(phi^2)
V <- sqrt(phi2/Dim) # V ou C de Cramer
if (0 <= V & V < 0.1) {gV <- "minimo"}
if (0.1 <= V & V < 0.3) {gV <- "pequeno"}
if (0.3 <= V & V < 0.5) {gV <- "intermediario"}
if (0.5 <= V & V <= 1.0) {gV <- "grande"}
cat("\nV de Cramer =", V, "\nGrau", gV, 
    "de dependencia entre as duas variaveis nominais\n")

cat("\nTeste de razao de chances (OR) robusto:\n")
resft <- fisher.test(TC,conf.level = 1-alfa) # Teste de OR robusto
print(resft)

# reabilita warnings
options(warn=0)

