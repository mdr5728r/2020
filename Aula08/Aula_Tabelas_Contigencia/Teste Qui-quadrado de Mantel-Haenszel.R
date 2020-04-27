# Teste Qui-quadrado de Mantel-Haenszel.R

# desabilita warnings
options(warn=-1)

# Tabela de contingencia 2x2 segmentada  
# Teste robusto da razao de chances (OR) de Mantel-Haenszel

# Relacao entre tabagismo e sobrevivência em 20 anos (1974-1994) 
# em 1.134 mulheres adultas do Reino Unido
# Delineamento: coorte
# Fonte:  APPLETON, D. R. et al. (1996) Ignoring a covariate:
# An example of Simpson's paradox. The American Statistician,
# 50(4): 340-1.

# A aspa inicial TEM que comecar na primeira coluna da linha
# e os espacamentos distintos dos dessa tabela podem causar 
# problemas de na geracao da tabela horizontalizada (ftable).
library(vcd)
library(rcompanion)
library(coin)
library(readxl)
library(tidyverse)
TCtmp <- read_excel("fumo_e_faixaetaria.xlsx")
TCtmp <- gather(TCtmp, var, Contagem, -Idade)
TCtmp <- separate(TCtmp, var, c('Exposicao','Desfecho')) 
TCS <- TCtmp %>% xtabs(Contagem ~ Exposicao + Desfecho + Idade, .)
print(TCS)

# Mantel-Haenszel test of the null that two nominal variables 
# are conditionally independent in each stratum, 
# assuming that there is no three-way interaction.
# Woolf test for homogeneity of ORs across strata on 2 x 2 x k tables:
# If significant, M-H test is not appropriate.
print(ftable(TCS)) # Display a flattened table (tabela horizontalizada)

cat("\nHomogeneidade dos ORs:\n")
print(vcd::woolf_test(TCS)) 

# Teste para tabela 2x2xK
cat("\nQui-quadrado de Mantel-Haenszel:\n")
print(mantelhaen.test(TCS, exact=TRUE)) # Teste exato de OR de Mantel-Haenszel

# Teste para tabela LxCxK
cat("\nTeste de Cochran-Mantel-Haenszel (coin::cmh_test):\n")
print(coin::cmh_test(TCS, distribution = approximate(nresample = 1e6)))

cat("\nAnalise estratificada por idade:\n")
print(rcompanion::groupwiseCMH(TCS,
             group   = 3,
             fisher  = TRUE,
             gtest   = FALSE,
             chisq   = FALSE,
             method  = "fdr",
             correct = "none",
             digits  = 3))
cat("\n")
print(ors <- vcd::oddsratio(TCS, log=FALSE)) # Show OR for each 2x2
cat("\n")
lnors <- vcd::oddsratio(TCS, log=TRUE) # Show ln(OR) for each 2x2
print(lnors)
print(summary(lnors))
print(confint(lnors))
plot(lnors, xlab = "Faixa Etaria", ylab = "ln(OR)")

# reabilita warnings
options(warn=0)
