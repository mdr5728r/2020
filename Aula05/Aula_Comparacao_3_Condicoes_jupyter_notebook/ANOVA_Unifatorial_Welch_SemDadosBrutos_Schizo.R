# Teste ANOVA unifatorial independente desbalanceado heterocesdático de Welch
# Modern Statistics for the Social and Behavioral Sciences in R - Wilcox - 2012, p. 442-6
nA <- 10
nB <- 10
nC <- 10
nD <- 10
mediaA <- 0.2760
mediaB <- 0.1956
mediaC <- 0.1495
mediaD <- 0.3997
dpA <- 0.4095
dpB <- 0.1808
dpC <- 0.2451
dpD <- 0.2382
n <- c(nA,nB,nC,nD)
J <- length(n)
media <- c(mediaA,mediaB,mediaC,mediaD)
dp <- c(dpA,dpB,dpC,dpD)
balanc <- max(dp)/min(dp)
var <- dp^2
w <- n/var
U <- sum(w)
X_til <- w%*%media/U
gl_num <- J - 1
A <- w%*%((media - X_til)^2)/(J-1)
B <- (2*(J - 2)/(J^2 - 1))*(((1 - w/U)/(n-1))%*%(1 - w/U))
gl_denom <- 1/(((3/2)/(J-2))*B)
F <- A/(1+B)
p <- pf(F,gl_num,gl_denom,lower.tail=FALSE)
razao <- max(dp)/min(dp)
eta2 <- gl_num*F/(gl_num*F + gl_denom)
# Elis P (2010) The essential guide to effect sizes. Cambridge 
if (eta2 <0.01) {mag_eta2<-c("Desprezivel")} 
if (eta2>=0.01 && eta2<0.06) {mag_eta2<-c("Pequeno")} 
if (eta2>=0.06 && eta2<0.14) {mag_eta2<-c("Intermediario")}
if (eta2>=0.14) {mag_eta2<-c("Grande")}
sink("ANOVA_Welch_SemDadosBrutos.txt")
cat("max(dp)/min(dp) = ", razao,"\n\n")
cat("Analise de significancia estatistica: valor-p\n")
cat("\tF(",gl_num,",",gl_denom,") = ",F,", p = ",p,"\n",sep="")
cat("Analise de significancia pratica: tamanho de efeito\n")
cat("\teta^2 = R^2 = ",eta2," (",mag_eta2,")\n",sep="")
sink()

