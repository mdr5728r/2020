# Teste t relacionado unilateral a esquerda sem dados brutos
n <- 16
mediaA <- 5.9
mediaB <- 4.8
dpA <- 1.2
dpB <- 1.8
r <- 0.3
alfa <- 0.05
dif <- mediaB - mediaA
df <- n - 1
ep <- sqrt((dpA^2 + dpB^2 - 2*r*dpA*dpB)/n)
t <- dif/ep
p <- pt(t,df)
eta2 <- t^2/(t^2 + df)
if (eta2 <0.01) {mag_eta2<-c("Desprezivel")} 
if (eta2>=0.01 && eta2<0.06) {mag_eta2<-c("Pequeno")} 
if (eta2>=0.06 && eta2<0.14) {mag_eta2<-c("Intermediario")}
if (eta2>=0.14) {mag_eta2<-c("Grande")}
sink("TestetRelacionadoUnilateralEsquerda_SemDadosBrutos.txt")
cat("Analise de significancia estatistica: valor-p\n")
cat("\tValor-p = ",p,"\n",sep="")
cat("Analise de significancia pratica: tamanho de efeito\n")
cat("\tEta^2 = ",eta2," (",mag_eta2,")",sep="")
sink()

