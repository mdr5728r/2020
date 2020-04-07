# ANOVA1f_indep_FisherWhite_sodio.R
# para ajustar este RScript para outros dados
# troque a planilha xlsx e substitua as palavras
# Instructor pela nova VI (fator)
# Sodium pela nova VD (resposta)

library(psych)
library(lattice)
library(car)
library(multcomp)
library(gplots)
library(emmeans)
library(readxl)

# suppress warnings
options(warn=-1)

TH <- read_excel("Nutricao3.xlsx")
TH$Instructor <- factor(TH$Instructor, levels=unique(TH$Instructor))

print(with(TH, psych::describeBy(Sodium,Instructor,digits=2)))
boxplot(Sodium~Instructor,data=TH,
        ylab=names(TH)[which(names(TH)=="Sodium")],
        xlab=names(TH)[which(names(TH)=="Instructor")]
)
print(grf <- lattice::xyplot(Sodium~Instructor, data=TH, type=c("p","a"),
                             jitter.x=TRUE, col="black"))
with(TH, gplots::plotmeans(Sodium~Instructor,
                           error.bars="conf.int", level=.95,
                           connect=FALSE,
                           ylab=names(TH)[which(names(TH)=="Sodium")],
                           xlab=names(TH)[which(names(TH)=="Instructor")],
                           main="IC95%",
                           barcol="black"))
car::densityPlot(Sodium~Instructor, data=TH, rug=TRUE, from=0, normalize=TRUE,
                 na.rm=TRUE, ylab="Densidade", col=c("black", "black", "black"))
cat("\n")

cat("\nANOVA unifatorial independente de Fisher",
    "\ncom ajuste para heterocedasticidade de White:\n\n")
alfa <- 0.05
VD <- names(TH)[which(names(TH)=="Sodium")]
VI <- names(TH)[which(names(TH)=="Instructor")]
cat("VD =", VD,"\n")
cat("Fator =", VI,"\n")
cat("\nAnalise de significancia estatistica: teste omnibus\n")
modelo <- lm(Sodium~Instructor, data=TH)
print(res <- car::Anova(modelo, type=2, white.adjust=TRUE))
cat("\nAnalise de significancia pratica: tamanho de efeito\n")
F <- res$F[1]
dfn <- res$Df[1]
dfd <- res$Df[2]
eta2 <- dfn*F/(dfn*F+dfd)
if (0 <= eta2 & eta2 < 0.1) {geta2 <- "minimo"}
if (0.1 <= eta2 & eta2 < 0.6) {geta2 <- "pequeno"}
if (0.6 <= eta2 & eta2 < 0.14) {geta2 <- "intermediario"}
if (0.14 <= eta2 & eta2 <= 1.0) {geta2 <- "grande"}
cat("- eta^2 =", eta2, "\nGrau", geta2,
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
         paste("Fc(",dfn,",",dfd,") = ",round(fc,3),sep=""), 
         paste("Fobs = ",round(F,3),"\n",
               "p = ",p,sep="") 
         ), 
       lwd=c(2,2,1,1), lty=c(1,2,3,4))
cat("\n\nSelecao de modelo\n")
R2aj <- (F-1)/((F-1)+dfd+1)
cat("- R^2 ajustado =", R2aj, "\n")
omega2 <- (F-1)/((F-1)+dfd+2)
cat("- omega^2 = ", omega2,"\n\n")
cat(paste("Teste post hoc\n"))
print(EMM <- emmeans::emmeans(modelo, "Instructor"))
print(grf <- plot(EMM, colors = "black",
                  main="Estimated Marginal Means",
                  xlab=VD,
                  ylab=VI))

# Testes post hoc
cat("\n\nTestes post hoc:\n")

# nomes do fator encurtados
TH$Instructor <- as.character(TH$Instructor)
fatores <- unique(as.character(TH$Instructor))
letra <- "A"
legenda <- c()
cat ("\nLegenda:\n")
for( f in 1:length(fatores))
{
  cat("\t",letra," ... ",fatores[f],"\n",sep="")
  legenda <- c(legenda,paste(letra," ... ",fatores[f],"\n",sep=""))
  TH$Instructor[TH$Instructor==fatores[f]] <- letra
  ascii <- strtoi(charToRaw(letra),16L)
  letra <- rawToChar(as.raw(ascii+1))
}
TH$Instructor <- as.factor(TH$Instructor)
modelo <- lm(Sodium~Instructor, data=TH)
mc.tuckey <- multcomp::glht(modelo, linfct = mcp(Instructor = "Tukey"))
print(mcs.tuckey <- summary(mc.tuckey, test=adjusted("bonferroni")))
multcomp::cld(mcs.tuckey, level=alfa, decreasing=TRUE)
plot(mc.tuckey,las=3)
legend("topleft",legenda,lwd=0,lty=0,cex=0.6,box.lwd=0, bg="transparent")
mc.dunnett <- multcomp::glht(modelo, linfct = mcp(Instructor = "Dunnett"))
mcs.dunnett <- summary(mc.dunnett, test=adjusted("bonferroni"))
print(mcs.dunnett)
plot(mc.dunnett,las=3)
legend("topleft",legenda,lwd=0,lty=0,cex=0.6,box.lwd=0, bg="transparent")

# enable warnings
options(warn=0)

