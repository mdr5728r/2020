# Teste qui-quadrado de Cochran-Armitage.R

# Cochran-Armitage test for trend
# Menarca precoce & espessura da dobra cutânea do tríceps  
# Fonte:  Beckles et al. (1985) International Journal of Obesity 9:127-35,
# apud Kirkwood & Sterne (2003): Chapter 17: Chi-squared tests for 2x2 and 
# larger contingency tables. 
library(readxl)
library(DescTools)

# desabilita warnings
options(warn=-1)

# Tabela de Dancey&Reidy (2019, p. 275)
TCtmp <- read_excel("menarca.xlsx")
TC <- data.matrix(TCtmp)
rownames(TC) <-as.character(unlist(TCtmp[,1]))
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

cat("\nTeste qui-quadrado de Cochran-Armitage:\n")
res <- DescTools::CochranArmitageTest(TC)
print(res)
x2ca <- res$statistic^2
p <- res$p.value
cat("\nX-squared trend =",x2ca, ", df = 1, ", "p-value =", p,"\n")

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

# reabilita warnings
options(warn=0)

