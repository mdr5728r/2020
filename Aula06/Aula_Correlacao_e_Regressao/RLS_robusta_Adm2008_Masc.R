# RLS_robusta_Adm2008_Masc.R

library(readxl)
library(RcmdrMisc)
library(estimatr)
library(car)
library(aplpack)
library(mcr)
Dados <- readxl::read_excel("Adm2008_Masc.xlsx")
Dados$Estatura <- 100*Dados$Estatura
res_boot <- mcr::mcreg(x=Dados$Estatura, y=Dados$MCT,
       mref.name = "Estatura (cm)", 
       mtest.name = "Massa Corporal Total (kg)", 
       method.reg = "LinReg", method.ci = "bootstrap",
      nsamples = 1e5, rng.seed = 123, na.rm = TRUE)
mcr::printSummary(res_boot)
mcr::getCoefficients(res_boot)
mcr::plot(res_boot)
res_classic <- mcr::mcreg(x=Dados$Estatura, y=Dados$MCT,
                       mref.name = "Estatura (cm)", 
                       mtest.name = "Massa Corporal Total (kg)", 
                       method.reg = "LinReg", method.ci = "analytical",
                       nsamples = 1e5, rng.seed = 123, na.rm = TRUE)
mcr::plot(res_classic)
compareFit(res_classic, res_boot)
ve <- colnames(Dados[,2])
vd <- colnames(Dados[,3])
# sink("RLS_robusta_Adm2008_Masc.txt")
# pdf("RLS_robusta_Adm2008_Masc.pdf")
head(Dados,6); cat("\n")
res_boot <- mcr::mcreg(x=Dados$Estatura, y=Dados$MCT,
                       mref.name = "Estatura (cm)", 
                       mtest.name = "Massa Corporal Total (kg)", 
                       method.reg = "LinReg", method.ci = "bootstrap",
                       nsamples = 1e5, rng.seed = 123, na.rm = TRUE)
mcr::printSummary(res_boot)
mcr::getCoefficients(res_boot)
mcr::plot(res_boot)
res_classic <- mcr::mcreg(x=Dados$Estatura, y=Dados$MCT,
                          mref.name = "Estatura (cm)", 
                          mtest.name = "Massa Corporal Total (kg)", 
                          method.reg = "LinReg", method.ci = "analytical",
                          nsamples = 1e5, rng.seed = 123, na.rm = TRUE)
mcr::plot(res_classic)
compareFit(res_classic, res_boot)
print(RcmdrMisc::numSummary(Dados[,2:3],
           statistics=c("mean", "sd", "quantiles"), 
           quantiles=c(0,.25,.5,.75,1)))
with(Dados, aplpack::bagplot(Estatura, MCT, na.rm = TRUE,
                             xlab="Estatura (cm)",
                             ylab="Massa Corporal Total (kg)"))
Estatura.media <- mean(Dados$Estatura, na.rm=TRUE)
MCT.media <- mean(Dados$MCT, na.rm=TRUE)
points(Estatura.media, MCT.media, pch=19)
print(car::scatterplot(MCT ~ Estatura, 
                 regLine=FALSE, smooth=FALSE, boxplots=FALSE, 
                 jitter=list(x=1, y=1), col="black", 
                 data=Dados))

points(Estatura.media, MCT.media, pch=19)
abline(v=Estatura.media, lty=2)
abline(h=MCT.media, lty=2)
rlsr <- estimatr::lm_robust(MCT ~ Estatura, data=Dados)
print(sumario.rlsr <- summary(rlsr))
F <- sumario.rlsr$fstatistic[1]
df <- sumario.rlsr$fstatistic[2]
dfErro <- sumario.rlsr$df.residual
b0 <- sumario.rlsr$coefficients[1]
b1 <- sumario.rlsr$coefficients[2]
s <- sd(Dados[[2]],na.rm=TRUE)
n <- sumario.rlsr$N
if (sumario.rlsr$r.squared == -Inf) {eta2 <- R2 <- 0} 
if (sumario.rlsr$r.squared != -Inf) {eta2 <- R2 <- sumario.rlsr$r.squared}
SEE <- s*sqrt(((n-1)/(n-2))*(1-eta2))
Rmult <- sqrt(R2)
if (b1<0) {Rmult <- -Rmult}
if (sumario.rlsr$adj.r.squared == -Inf) {R2aj <- 0} 
if (sumario.rlsr$adj.r.squared != -Inf) {R2aj <- sumario.rlsr$adj.r.squared}
cat("\ndesvio-padrao de ",vd," = ",s,"\nErro-padrao da estimativa de ",vd," = ",
    SEE,"\n", sep="")
cat("R multiplo = ",Rmult,"\nR^2 = coefic. de determinacao = ", R2,"\n", sep="")
cat("Eta^2 de ",ve," = ",eta2,"\n\n",sep="")
# Usar estimatr::predict ...
car::confidenceEllipse(rlsr,levels=.95,Scheffe=FALSE,fill=TRUE,fill.alpha=0.1)
lid <- min(Dados$Estatura,na.rm=TRUE)
lsd <- max(Dados$Estatura,na.rm=TRUE)
Estatura.centr <- Dados$Estatura - mean(Dados$Estatura,na.rm=TRUE)
rlsr.centr <- estimatr::lm_robust(MCT ~ Estatura.centr, data=Dados)
b0.centr <- rlsr.centr$coefficients[1]
cat("MCT.Media =",b0,"+",b1,"* Estatura [",lid,",",lsd,"]\n\n")
cat("MCT.Media =",b0.centr,"+",b1,"* Estatura.Centrada [",
    lid - mean(Dados$Estatura,na.rm=TRUE),",",
    lsd - mean(Dados$Estatura,na.rm=TRUE),"]\n\n")
# dev.off()
# sink()

