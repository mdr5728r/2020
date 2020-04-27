# Criando tabela de contingencia 2x2 e realizando
# testes qui-quadrado de Pearson e de razao de chances (OR)

library("DescTools")

# arquivos com resultados
file_text <- "tabelacontingencia2x2_capacetetrauma.txt"

# Usar apenas espaco em branco para formatar tabela de contigencia
fator_exposicao <- "Uso do capacete"
Tabela <- ("
 Capacete      Trauma   Nao_trauma
 Sim           17       138
 Nao           130      508
")

# Nao edite daqui em diante
TC <- as.matrix(read.table(textConnection(Tabela), 
                               header=TRUE, row.names=1))

sink(file_text)

cat("\n")
cat("------------------------------\n")
cat("Testes qui-quadrado de Pearson\n")
cat("------------------------------\n")
cat("\n")

cat("Fator de exposicao: ",fator_exposicao,"\n")
print(TC)
# GARCIA-PEREZ, MA & NUNEZ-ANTON, V (2003) 
# Cellwise residual analysis in two-way contingency tables.
# Educational and Psychological Measurement 63(5): 825-839
cat("\nX^2 critico de 95% =",qchisq(p=0.95,df=1),"\n")
cat("\n----------\nTeste qui-quadrado assintotico sem correcao de Yates\n")
test <- chisq.test(TC,correct=FALSE)
print (test)
cat("\n----------\nTeste qui-quadrado assintotico com correcao de Yates\n")
test <- chisq.test(TC,correct=TRUE)
print (test)
cat("\n----------\nTeste qui-quadrado exato (robusto)\n")
res <- chisq.test(TC,correct=FALSE,simulate.p.value = TRUE, B=1e6)
print(res)

cat("\n----------\nResiduos ajustados standarizados corrigidos por momento (MCSTARs)\n")
N <- sum(res$observed)
nL <- nrow(TC) # ou dim(TC)[1]
nC <- ncol(TC) # ou dim(TC)[2]
df <- (nL-1)*(nC-1)
STAR <- res$stdres # STandardized Adjusted Residual (STAR)
MCSTAR <- STAR/(sqrt((1-1/nL)*(1-1/nC))) # Moment-correct (MCSTAR)
print(MCSTAR)
alfaBonf <- 0.05/df
zcrit <- abs(qnorm(alfaBonf/2))
cat("|MCSTAR critico| (alfaBonferroni=5%/",df,") =",zcrit,"\n\n")
X2 <- res$statistic # estatistica de teste qui-quadrado
phi2 <- X2/N # phi ou w de Cohen
Dim <- min(nL,nC)-1 # dimensoes da TC: dim = max(phi^2)
V <- sqrt(phi2/Dim) # V ou C de Cramer
cat("V de Cramer =",V,"\n")
ncp <- X2 # parametro de nao-centralidade
poder <- 1-pchisq(qchisq(0.95,df),df,ncp)
cat("Poder observado =",poder,"\n")

cat("\nTeste de razao de chances (OR) robusto\n")
resft <- fisher.test(TC) # Teste de OR robusto
print(resft)
OR <- resft$estimate
cat("Razao de chances (OR) =",OR,"\n")
preval <- 0.05
RR <- DescTools::ORToRelRisk(OR,preval)
cat("Razao de riscos (RR) a partir de OR\n(",
    preval, "de prevalencia do desfecho na populacao nao-exposta)\n",
    "RR =",RR,"\n")
# Personality Project - Revelle – 2014
# Covariance, Regression, and Correlation in R - Chapter 4
d <- abs(log(OR)/1.81)
cat("abs(d de Cohen) =",d,"\n")
r <- d/sqrt(d^2+4)
cat("abs(correl) =",r,"\n\n")
# EDWARDES, MD & BALTZAN, M (2000) The generalization of the odds ratio,
# risk ratio and risk difference to rxk tables.
# Statistics in Medicine 19:1901-14.
gama <- DescTools::GoodmanKruskalGamma(TC)
ORg <- (1+gama)/(1-gama)
cat("Gama de Goodman-Kruskal =", gama,"\n")
cat("OR generalizado = (1+",gama,")/(1-",gama,") =", ORg,"\n")
lnORg <- log(ORg)
cat("ln(OR generalizado) =",lnORg,"\n")
RRg <- DescTools::ORToRelRisk(ORg,preval)
cat("Razao de riscos generalizada (RRg) a partir de ORg\n",
    "(ZHANG and YU,1998)\n",
    "(",preval, "de prevalencia do desfecho na populacao nao-exposta)\n",
    "RRg =",RRg,"\n")
dg <- abs(log(ORg)/1.81)
cat("abs(d de Cohen) =",dg,"\n")
rg <- dg/sqrt(dg^2+4)
cat("abs(correl) =",rg,"\n")

# fecha o arquivo
sink()
cat ("\nResultados armazenados em ",file_text,"\n", sep="")