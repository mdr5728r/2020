# ANOVA1f_dep_balanc_sodio.R
# para ajustar este RScript para outros dados
# troque a planilha xlsx e substitua as palavras
# Instructor pela nova VI (fator)
# Sodium pela nova VD (resposta)

library(psych)
library(lattice)
library(car)
library(lmerTest)
library(ez)
library(readxl)

# suppress warnings
options(warn=-1)

TH <- read_excel("Nutricao3par.xlsx")
TH$Instructor <- factor(TH$Instructor, levels=unique(TH$Instructor))

print(with(TH, psych::describeBy(Sodium,Instructor,digits=2)))
boxplot(Sodium~Instructor,data=TH,
        ylab=names(TH)[which(names(TH)=="Sodium")],
        xlab=names(TH)[which(names(TH)=="Instructor")]
)
print(grf <- lattice::xyplot(Sodium~Instructor, data=TH, type=c("p","a"),
                             jitter.x=TRUE, col="black"))
car::densityPlot(Sodium~Instructor, data=TH, rug=TRUE, from=0, normalize=TRUE,
                 na.rm=TRUE, ylab="Densidade", col=c("black", "black", "black"))
cat("\n")

cat(paste("\nTeste omnibus por GLMM com efeito aleatorio\n"))
alfa <- 0.05
VD <- names(TH)[which(names(TH)=="Sodium")]
VI <- names(TH)[which(names(TH)=="Instructor")]
cat("VD =", VD,"\n")
cat("Fator =", VI,"\n")
cat("\nAnalise de significancia estatistica: teste omnibus\n")
res <- lmerTest::lmer(Sodium~Instructor + (1|Student), data=TH, REML=TRUE)
cat("\n"); print(res1 <- stats::anova(res)); cat("\n")
print(lmerTest::rand(res))
cat(paste("\nTeste post hoc\n\n"))
print(out <- lmerTest::difflsmeans(res))
dfn <- as.numeric(res1[3])
dfd <- as.numeric(res1[4])
F <- as.numeric(res1[5])
cat("\nAnalise de significancia pratica: tamanho de efeito\n")
eta2 <- dfn*F/(dfn*F+dfd)
if (0 <= eta2 & eta2 < 0.1) {geta2 <- "minimo"}
if (0.1 <= eta2 & eta2 < 0.6) {geta2 <- "pequeno"}
if (0.6 <= eta2 & eta2 < 0.14) {geta2 <- "intermediario"}
if (0.14 <= eta2 & eta2 <= 1.0) {geta2 <- "grande"}
cat("\neta^2 =", eta2, "\nGrau", geta2,
    "de explicacao da variancia da VD", VD,"pela VI", VI,"\n")
f2 <- eta2/(1-eta2) # tamanho de efeito f de Cohen
ncp <- dfd*f2 # parametro de nao-centralidade
fc <- qf(1-alfa, dfn, dfd, 0)
p <- 1-pf(F,dfn,dfd,0)
if (p < 1e-4)
{
  p <- sprintf("%.2e",p)
} else
{
  p <- sprintf("%.4f",p)
}
f <- seq(0,2*ncp,0.01)
densf <- df(f, dfn, dfd, 0)
plot(f, densf, xlab="F", ylab="densidade", lwd=2, type="l")
densf <- df(f, dfn, dfd, ncp)
lines(f,densf, lwd=2, lty=2)
abline(v=fc, lty=3)
abline(v=F, lty=4)
legend("topright",
       c("H0", "Obs", 
         paste("Fc(",dfn,",",round(dfd,3),") = ",round(fc,3),sep=""), 
         paste("Fobs = ",round(F,3),"\n",
               "p = ",p,sep="") 
         ), 
       lwd=c(2,2,1,1), lty=c(1,2,3,4))
cat("\n\nSelecao de modelo\n")
R2aj <- (F-1)/((F-1)+dfd+1)
cat("- R^2 ajustado =", R2aj, "\n")
omega2 <- (F-1)/((F-1)+dfd+2)
cat("- omega^2 = ", omega2,"\n\n")
cat("Teste omnibus por GLM univariado\n")
# ezANOVA: apenas para medidas repetidas balanceadas
print(res <- ez::ezANOVA(data=TH, dv=Sodium, wid=Student, within=Instructor,
                         detailed=TRUE, type=3))
# enable warnings
options(warn=0)
