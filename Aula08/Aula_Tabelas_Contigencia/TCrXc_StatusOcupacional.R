# TCrXc_StatusOcupacional.R

library(datasets)
library(DescTools)

eiras.show.MCSTAR <- function (MCSTAR, zcrit)
{
  MCSTARtxt <- round(MCSTAR,3)
  for (r in 1:nrow(MCSTAR))
  {
    for (c in 1:nrow(MCSTAR))
    {
      if (MCSTAR[r,c] > zcrit)
      {
        MCSTARtxt[r,c] <- paste("#",MCSTARtxt[r,c],sep="")
      } else
      {
        MCSTARtxt[r,c] <- paste(" ",MCSTARtxt[r,c],sep="")
      }
    }
  }
  return(MCSTARtxt)
}

# desabilita warnings
options(warn=-1)

cat("\n------------------------------------------\n")
cat("Dados:")
cat("\n------------------------------------------\n")
# Duncan, O.D. (1979), How Destination Depends on Origin in the 
# Occupational Mobility Table. American Journal of Sociology 84, 793-803.
tcrxc <- datasets::occupationalStatus
# para recolocar as categorias originais
categories <- c("I","II","III","IV","Va","Vb","VI","VII")
rownames(tcrxc) <- categories
colnames(tcrxc) <- categories
prmatrix(tcrxc)

cat("\n------------------------------------------\n")
cat("Analise de significancia estatistica:")
cat("\n------------------------------------------\n")
alfa <- 0.05

cat("\nTeste qui-quadrado de Pearson exato:\n")
print(res <- chisq.test(tcrxc,simulate.p.value=TRUE,B=1e6))
N <- sum(res$observed)
nL <- nrow(tcrxc) # ou dim(TC)[1]
nC <- ncol(tcrxc) # ou dim(TC)[2]
df <- (nL-1)*(nC-1)
cat("X^2 critico de 95% = ",qchisq(p=1-alfa,df=df), "\n", sep="")
X2 <- res$statistic # estatistica de teste qui-quadrado
cat("Graus de liberdade (nao fornecidos por bootstrapping): ", df, "\n", sep="")
cat("Heuristica de significancia (rej. H0 se X^2/gl > 2): ", X2/df, "\n", sep="")

cat("\n------------------------------------------\n")
cat("Analise post hoc:")
cat("\n------------------------------------------\n")
cat("\nResiduos ajustados standarizados corrigidos por momento (MCSTARs):\n")
STAR <- res$stdres # STandardized Adjusted Residual (STAR)
MCSTAR <- STAR/(sqrt((1-1/nL)*(1-1/nC))) # Moment-correct (MCSTAR)
alfaBonf <- alfa/df
zcrit <- abs(qnorm(alfaBonf/2))
print(eiras.show.MCSTAR(MCSTAR,zcrit))
cat("\n|MCSTAR critico| (alfaBonferroni=5%/",df,") = ",zcrit,"\n", sep="")
phi2 <- X2/N # phi ou w de Cohen
Dim <- min(nL,nC)-1 # dimensoes da TC: dim = max(phi^2)

cat("\n------------------------------------------\n")
cat("Analise de significancia pratica:")
cat("\n------------------------------------------\n")
V <- sqrt(phi2/Dim) # V ou C de Cramer
if (0 <= V & V < 0.1) {gV <- "minimo"}
if (0.1 <= V & V < 0.3) {gV <- "pequeno"}
if (0.3 <= V & V < 0.5) {gV <- "intermediario"}
if (0.5 <= V & V <= 1.0) {gV <- "grande"}
cat("\nV de Cramer =", V, "\nGrau", gV, 
    "de dependencia entre as duas variaveis nominais\n")

# Goodman, L. A. (1979) Simple Models for the Analysis of Association 
# in Cross-Classifications having Ordered Categories. 
# J. Am. Stat. Assoc., 74 (367), 537–552.
gama <- DescTools::GoodmanKruskalGamma(tcrxc)
cat("\nGama de Goodman-Kruskal = ", gama,"\n", sep="")

# EDWARDES, MD & BALTZAN, M (2000) The generalization of the odds ratio,
# rik ratio and risk difference to rxk tables.
# Statistics in Medicine 19:1901-14.
ORg <- (1+gama)/(1-gama)
cat("\nOR generalizado = (1+",gama,")/(1-",gama,") = ", ORg,"\n", sep="")

# reabilita warnings
options(warn=0)

