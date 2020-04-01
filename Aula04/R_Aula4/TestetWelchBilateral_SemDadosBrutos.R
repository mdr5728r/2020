# Teste t de Welch bilateral sem dados brutos
nA <- 16
nB <- 25
mediaA <- 4.8
mediaB <- 5.6
dpA <- 1.8
dpB <- 1.2
alfa <- 0.05
dif <- mediaB - mediaA
dfA <- nA - 1
dfB <- nB - 1
ep2A <- dpA^2/nA; ep2B <- dpB^2/nB
df <- ((ep2A+ep2B)^2)/((ep2A^2)/dfA+(ep2B^2)/dfB)
t <- dif/sqrt(ep2A+ep2B)
p <- 2*pt(-abs(t),df)
eta2 <- t^2/(t^2 + df)
if (eta2 <0.01) {mag_eta2<-c("Desprezivel")} 
if (eta2>=0.01 && eta2<0.06) {mag_eta2<-c("Pequeno")} 
if (eta2>=0.06 && eta2<0.14) {mag_eta2<-c("Intermediario")}
if (eta2>=0.14) {mag_eta2<-c("Grande")}
sink("TestetWelchBilateral_SemDadosBrutos.txt")
cat("Analise de significancia estatistica: valor-p\n")
cat("\tValor-p = ",p,"\n",sep="")
cat("Analise de significancia pratica: tamanho de efeito\n")
cat("\tEta^2 = ",eta2," (",mag_eta2,")",sep="")
sink()