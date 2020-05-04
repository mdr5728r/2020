library(epiR)
library(rcompanion)
library(diffdepprop)
sink("TC2x2_intrapartic.txt")
# Foram analisadas 315 amostras usando os métodos Bell e 
# Kato-Katz para detecção ovos de Schistosoma mansoni nas fezes.
# Sleigh et al. (1982) Transactions of the Royal Society of 
# Tropical Medicine and Hygiene 76: 403-6.
Tabela <- ("
  BellxKK P   N
  P       184 54
  N       14  63
")
print(TC <- as.matrix(read.table(textConnection(Tabela), 
                                 header=TRUE, row.names=1)))
cat("\nTeste de concordancia: delineamento intraparticipantes\n")
cat("\nSoftware de referencia: IBM SPSS Statistics 24\n")
cat("Teste kappa de Cohen\n")
cat("H0: kappa = 0 vs H1: kappa > 0\n",sep="")
res <- epiR::epi.kappa(TC,alternative="greater")
print(res$kappa)
print(res$z)
z <- as.numeric(res$z[1])
cat("z^2 = ", z^2,"\n",sep="")
out <- chisq.test(TC, correct=FALSE)
a <- TC[1,1]; b <- TC[1,2]; c <- TC[2,1]; d <- TC[2,2]; n <- sum(TC)
pae <- out$expected[1,1]/n
pbe <- out$expected[1,2]/n
pce <- out$expected[2,1]/n
pde <- out$expected[2,2]/n
kmax <- (min(pae+pbe,pae+pce)+min(pce+pde,pbe+pde)-pae-pde)/(1-pae-pde)
cat("kappa de Cohen maximo = ",kmax,"\n",sep="")
k <- as.numeric(res$kappa[1])
if(k>0){
  k <- k/kmax
  cat("kappa de Cohen corrigido pelo kappa maximo = ",k,"\n",sep="")
}
if(k<=0){
  cat("\nkappa de Cohen não-positivo\n")
  cat("kappa de Cohen nao corrigido pelo kappa maximo = ",k,"\n",sep="")
}
# Landis & Koch (1997)
if (0 <= k & k < 0.1) {gkl <- "Poor"}
if (0.1 <= k & k < 0.2) {gkl <- "Slight"}
if (0.2 <= k & k < 0.4) {gkl <- "Fair"}
if (0.4 <= k & k < 0.6) {gkl <- "Moderate"}
if (0.6 <= k & k < 0.8) {gkl <- "Substancial"}
if (0.8 <= k & k <= 1.0) {gkl <- "Almost perfect"}
cat("Grau de concordancia entre dois metodos/avaliadores = ",gkl,"\n",sep="")
# HRIPCSAK, G & ROTHSCHILD, AS (2005) Agreement, the F-measure, and reliabilty
# in information retrieval. J Am Med Inform Assoc 12:296-8.
F <- 2*a/(2*a+b+c)
cat("F-measure = ",F,"\n",sep="")
V <- rcompanion::cramerV(TC,ci=TRUE,R=1e4)
cat("fi = V de Cramer = ",V$Cramer.V,"\n",sep="")
if(!is.na(V$lower.ci) & !is.na(V$lower.ci)){
if (0 <= V$lower.ci & V$lower.ci < 0.1) {gVl <- "minimo"}
if (0.1 <= V$lower.ci & V$lower.ci < 0.3) {gVl <- "pequeno"}
if (0.3 <= V$lower.ci & V$lower.ci < 0.5) {gVl <- "intermediario"}
if (0.5 <= V$lower.ci & V$lower.ci <= 1.0) {gVl <- "grande"}
if (0 <= V$upper.ci & V$upper.ci < 0.1) {gVu <- "minimo"}
if (0.1 <= V$upper.ci & V$upper.ci < 0.3) {gVu <- "pequeno"}
if (0.3 <= V$upper.ci & V$upper.ci < 0.5) {gVu <- "intermediario"}
if (0.5 <= V$upper.ci & V$upper.ci <= 1.0) {gVu <- "grande"}
cat("IC95(V de Cramer) = [", V$lower.ci, ", ", V$upper.ci,"]\n",sep="")
cat("Grau de dependencia entre as duas variaveis nominais = 
    [", gVl, ", ", gVu, "]\n",sep="")
}
alfa <- .05
# Bonnet, DG & Price, RM (2012) Adjusted Wald confidence interval for a
# difference of binomial proportion based ond paired data
# Journal of Edicational and Behavioral Statistics 37(4): 479-88.
cat("\nH0: pi_+1 - pi_1+ = 0 vs. H0: pi_+1 - pi_1+ != 0\n")
diffdepprop::diffpci(a=a, b=b, c=c, d=d, n=n, alpha=alfa)
#        Method Estimator lower limit upper limit
# 1        Wald 0.1269841  0.07762879   0.1763395
# BellxKK: A probabilidade média do teste Bell ser positivo é 12.7% maior 
# que a probabilidade do teste KK ser positivo.
sink()

# # GWET, KL (2008) Computing inter-rater reliability and its variance 
# # in the presence of high agreement. British Journal of Mathematical and
# # Statistical Psychology 61: 29-48.
# Tabela <- ("
#      P   N
#   P  118 5  
#   N  2   0
# ")



