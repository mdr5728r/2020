# Teste ANOVA unifatorial independente desbalanceado de Fisher
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
n <- c(nA,nB,nC,nF)
gl <- n - 1 
gl_num <- length(n) - 1
gl_denom <- sum(gl)
media <- c(mediaA,mediaB,mediaC,mediaD)
dp <- c(dpA,dpB,dpC,dpD)
media_grande <- mean(media)
s2_entre <- (n%*%((media - media_grande)^2))/gl_num
var <- dp^2
s2_dentro <- (gl%*%var)/gl_denom
F <- s2_entre/s2_dentro
p <- pf(F,gl_num,gl_denom,lower.tail=FALSE)
eta2 <- gl_num*F/(gl_num*F + gl_denom)
razao <- max(dp)/min(dp)
# Elis P (2010) The essential guide to effect sizes. Cambridge 
if (eta2 <0.01) {mag_eta2<-c("Desprezivel")} 
if (eta2>=0.01 && eta2<0.06) {mag_eta2<-c("Pequeno")} 
if (eta2>=0.06 && eta2<0.14) {mag_eta2<-c("Intermediario")}
if (eta2>=0.14) {mag_eta2<-c("Grande")}
sink("ANOVA_Fisher_SemDadosBrutos_Schizo.txt")
cat("max(dp)/min(dp) = ", razao,"\n\n")
cat("Analise de significancia estatistica: valor-p\n")
cat("\tF(",gl_num,",",gl_denom,") = ",F,", p = ",p,"\n",sep="")
cat("Analise de significancia pratica: tamanho de efeito\n")
cat("\teta^2 = R^2 = ",eta2," (",mag_eta2,")\n",sep="")
sink()
