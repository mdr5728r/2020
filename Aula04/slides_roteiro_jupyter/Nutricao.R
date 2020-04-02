# Teste t independente
# Comparação de duas condicoes independentes

library(readxl)
library(car)
library(lattice)
library(ggplot2)
library(rcompanion)

# suppress warnings
options(warn=-1)


# In the following example, Brendon Small and Coach McGuirk 
# have their SNAP-Ed students keep diaries of what they eat for a week, 
# and then calculate the daily sodium intake in milligrams. 
# Since the classes have received different nutrition education programs, 
# they want to see if the mean sodium intake is the same for both classes.
# Fonte: Salvatore S. Mangiafico http://rcompanion.org/handbook/I_03.html
# "A quantidade diária de sódio disponível para consumo nos
# domicílios brasileiros foi de 4,7 g para ingestão diária de 2.000 kcal, 
# mantendose mais de duas vezes superior ao limite recomendado de 
# ingestão desse nutriente." 
# Fonte: SARNO et al. (2013) Estimativa de consumo de sodio 
# pela populacao brasileira 2008-9
# Rev Saude Publica 47(3):571-8


# H0: mu_sal_Small  = mu_sal_McGuirk
# H1: mu_sal_Small != mu_sal_McGuirk
alfa <- 0.05 # nivel de significancia adotado
# nome do arquivo de saída (se NA, saida em tela)
cat ("Voce pode guardar os resultados em um arquivo\n") 
cat ("(deixando em branco, a saida eh feita na tela)\n") 
filename <- readline(prompt="Nome do arquivo: ")
if (filename == "") {filename <- NA}

# dados
Dtfrm <- read_excel("Nutricao.xlsx")
# a primeira coluna nao deve ser tratada como numero
Dtfrm$Instructor <- as.factor(Dtfrm$Instructor)
Dtfrm$Instructor <- factor(Dtfrm$Instructor, levels=unique(Dtfrm$Instructor))

res_sodiumbyinstructor <- summary.data.frame(Dtfrm, digits=2)
print (res_sodiumbyinstructor)


# Graficos (estatistica descritiva)
par(mfrow = c(1, 1))
filegrf <- c()
filenum <- 0
# boxplot de cada conjunto de dados
if (!is.na(filename))
{
  filenum <- filenum+1
  filegrf <- c(filegrf, paste(filename,"_boxplot.png", sep="")); 
  png(filegrf[filenum], width = 600, height = 400)
}
boxplot(Sodium ~ Instructor, data=Dtfrm, ylab=names(Dtfrm)[3], 
        xlab="Instructor")
if (!is.na(filename))
{
  dev.off()
}
# lattice de cada conjunto de dados
if (!is.na(filename))
{
  filenum <- filenum+1
  filegrf <- c(filegrf, paste(filename,"_dots.png", sep="")); 
  png(filegrf[filenum], width = 600, height = 400)
}

# dots
grf <- lattice::xyplot(Sodium ~ Instructor, data=Dtfrm, type=c("p","a"),jitter.x=TRUE)
print (grf)
if (!is.na(filename))
{
  dev.off()
}
# densityplot de cada conjunto de dados
if (!is.na(filename))
{
  filenum <- filenum+1
  filegrf <- c(filegrf, paste(filename,"_densplot.png", sep="")); 
  png(filegrf[filenum], width = 600, height = 400)
}
grf <- car::densityPlot(Sodium~Instructor, data=Dtfrm, rug=TRUE)
print (grf)
if (!is.na(filename))
{
  dev.off()
}
# IC medias marginais
if (!is.na(filename))
{
  filenum <- filenum+1
  filegrf <- c(filegrf, paste(filename,"_ICmm.png", sep="")); 
  png(filegrf[filenum], width = 600, height = 400)
}

SumMM <- rcompanion::groupwiseMean(Sodium ~ Instructor,
                       data   = Dtfrm,
                       conf   = 0.95,
                       digits = 3,
                       traditional = FALSE,
                       percentile  = TRUE)

grf <- ggplot2::ggplot(SumMM, ggplot2::aes(x = Instructor, y = Mean)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = Percentile.lower,
                                      ymax = Percentile.upper),
                         width = 0.05, size  = 0.5) +
  ggplot2::geom_point(shape = 15, 
                      size  = 4) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title   = ggplot2::element_text(face  = "bold")) +
  ylab(names(Dtfrm)[3])
print (grf)
if (!is.na(filename))
{
  dev.off()
}

# O teste t de Welch [sic] é o default de t.test{stats}
# O teste é t de Satterthwaite, conforme STATA e
# SATTERTHWAITE, FE (1946) Approximate distribution of estimates of variance components.
# Biometrics Bulletin, 2(6): 110-114.
# WELCH, BL (1947) The generalization of ‘Student’s’ problem when several different population variances 
# are involved. Biometrika, 34(1/2): 28-35.

# separa os dois instrutores
SodiumBS <- subset(Dtfrm,select=Sodium,subset=Instructor=="Brendon Small",drop=TRUE)
SodiumCM <- subset(Dtfrm,select=Sodium,subset=Instructor=="Coach McGuirk",drop=TRUE)
# dados da amostra
nA <- sum(!is.na(SodiumBS)) 
nB <- sum(!is.na(SodiumCM))

# significancia estatistica
t_out <- t.test(Sodium ~ Instructor, data = Dtfrm)

# significancia pratica
t <- t_out$statistic # estatistica de teste t
df <- t_out$parameter # graus de liberdade
dfefic <- (df-min(nA,nB))/(nA+nB-2-min(nA,nB))

cat("\nTamanho das amostras: \n", sep="")
cat ("\tBrendon Small: n = ", nA, "\n", sep="") 
cat ("\tCoach McGuirk: n = ", nB, "\n", sep="") 

cat ("\n")
cat("Analise de significancia estatistica: valor-p\n")
cat("Teste t de Satterthwaite\n")
print(t_out)

cat("Eficiencia do numero de graus de liberdade =",dfefic,"\n\n")

# distribuicao t sob H0 (central: ncp = 0)
tH0 <- rt(1e6, df)
dtH0 <- density(tH0)
# distribuicao t sob H1, ncp = t
# ncp
F <- t^2 # estatistica de teste F de Fisher
eta2 <- F/(F+df)
f2 <- eta2/(1-eta2) # f de Cohen
ncp <- df*f2 # parametro de nao-centralidade: ncp = F
tH1 <- rt(1e6, df, ncp)
dtH1 <- density(tH1)

if (!is.na(filename))
{
  filetxt <- paste(filename,".txt", sep="")
  sink (filetxt)
  cat ("---------------------------------------\n")
  cat("Processando:",filename,"\n")
  cat ("---------------------------------------\n\n")
} 

# Referencial teorico da distribuicao t central e nao-central
# media e dp das dist. t central e nao-central
mediaH0 <- 0
dpH0 <- sqrt(df/(df-2))
beta <- sqrt(df/2)*gamma((df-1)/2)/gamma(df/2)
mediaH1 <- ncp*beta
dpH1 <- sqrt((df*(1+ncp^2)/(df-2))-mediaH1^2)

cat("\nReferencial teorico:\n")
cat("\tvalor-p =",t_out$p.value,"\n")
cat("sob H0: distribuicao t com media = ",mediaH0," e d.p. = ",dpH0,"\n",sep="")
cat("sob H1: distribuicao t com media = ",mediaH1," e d.p. = ",dpH1,"\n",sep="")
cat("\n")

par(mfrow = c(2, 1)) # grid 2 linhas e 1 coluna
if (!is.na(filename))
{
  filenum <- filenum+1
  filegrf <- c(filegrf, paste(filename,"_t.png", sep="")); 
  png(filegrf[filenum], width = 600, height = 800)
}
for (g in 1:2)
{
  # limites de x
  min_x <- min(mediaH0-3*dpH0, mediaH1-3*dpH1)
  max_x <- max(mediaH0+3*dpH0, mediaH1+3*dpH1)
  if (g == 1)
  {
    plot(dtH0,
         main=paste("Teste t independente\ndf =",round(df,3),", t =",round(t,5),", alfa =",alfa),
         xlab="t", 
         xlim=c(min_x,max_x),
         lwd=1, lty=1
    )
  }
  if (g == 2)
  {
    plot(dtH0,
         main=NA,
         xlab="t", 
         xlim=c(min_x,max_x),
         lwd=1, lty=1
    )
  }
  qalfa <- qt(c(alfa/2,1-alfa/2),df,0)
  abline(v=qalfa[1], lty = 3)
  abline(v=qalfa[2], lty = 3)
  if (g==1)
  {
    abline(v=abs(t),lwd=1,lty=2)
    abline(v=-abs(t),lwd=1,lty=2)
    # area do valor p
    polx <- dtH0$x[dtH0$x>=abs(t)]; polx <- c(min(polx),polx,max(polx))
    poly <- dtH0$y[dtH0$x>=abs(t)]; poly <- c(0,poly,0)
    polygon(polx,poly,border="#EE802622",col="#EE802688",lwd=5)
    polx <- dtH0$x[dtH0$x<=-abs(t)]; polx <- c(min(polx),polx,max(polx))
    poly <- dtH0$y[dtH0$x<=-abs(t)]; poly <- c(0,poly,0)
    polygon(polx,poly,border="#EE802622",col="#EE802688",lwd=5)
  }
  if (g==2)
  {
    # H1
    lines(dtH1,lwd=3,lty=1)
    # area alfa
    polx <- dtH0$x[dtH0$x<=qalfa[1]]; polx <- c(min(polx),polx,max(polx))
    poly <- dtH0$y[dtH0$x<=qalfa[1]]; poly <- c(0,poly,0)
    polygon(polx,poly,border="#a3261b22",col="#a3261b88",lwd=5)
    polx <- dtH0$x[dtH0$x>=qalfa[2]]; polx <- c(min(polx),polx,max(polx))
    poly <- dtH0$y[dtH0$x>=qalfa[2]]; poly <- c(0,poly,0)
    polygon(polx,poly,border="#a3261b22",col="#a3261b88",lwd=5)
    # area beta
    polx <- dtH1$x[dtH1$x>=qalfa[1] & dtH1$x<=qalfa[2]]; polx <- c(min(polx),polx,max(polx))
    poly <- dtH1$y[dtH1$x>=qalfa[1] & dtH1$x<=qalfa[2]]; poly <- c(0,poly,0)
    polygon(polx,poly,border="#4EB26522",col="#4EB26588",lwd=8)
  }
  
  # legenda
  if (g==1)
  {
    p_txt <- t_out$p.value;
    if (p_txt > 0.001) 
    {
      p_txt <- round(p_txt,4)
    } else
    {
      p_txt <- format(format(p_txt, scientific = TRUE, digits = 4)) 
    } 
    legend ("topright",
            c("H0","t obs.","t crit.",paste("p=",p_txt,sep="")), 
            lwd=c(1,1,1,10),
            lty=c(1,2,3,1),
            pch=NA,
            col=c("black","black","black","#EE802688"),
            box.lwd=0, bg="transparent")
  } 
  if (g==2)
  {
    legend ("topright",
            c("H0","H1","t crit.","alfa","beta"), 
            lwd=c(1,3,1,10,10),
            lty=c(1,1,3,1,1),
            pch=NA,
            col=c("black","black","black","#a3261b88","#4EB26588"),
            box.lwd=0, bg="transparent")
  } 
}
if (!is.na(filename))
{
  dev.off()
}

# Elis P (2010) The essential guide to effect sizes. Cambrige 
if (eta2 <0.01) {mag_eta2<-c("Desprezivel")} 
if (eta2>=0.01 && eta2<0.06) {mag_eta2<-c("Pequeno")} 
if (eta2>=0.06 && eta2<0.14) {mag_eta2<-c("Intermediario")}
if (eta2>=0.14) {mag_eta2<-c("Grande")}

d <- abs(t)/sqrt(1/((1/nA)+(1/nB)))
# Sawilowsky, S (2009) New effect size rules of thumb. Journal of Modern Applied Statistical Methods 8(2): 467-74.
if (d<0.01) {mag_Cohen<-c("Desprezivel")} 
if (d>=0.01 && d<0.2) {mag_Cohen<-c("Muito pequeno")} 
if (d>=0.2 && d<0.5) {mag_Cohen<-c("Pequeno")}
if (d>=0.5 && d<0.8) {mag_Cohen<-c("Intermediario")}
if (d>=0.8 && d<1.2) {mag_Cohen<-c("Grande")}
if (d>=1.2 && d<2) {mag_Cohen<-c("Muito grande")} 
if (d>=2) {mag_Cohen<-c("Enorme")}
g <- d*(1-3/(4*df-1))
if (g<0.01) {mag_Hedges<-c("Desprezivel")} 
if (g>=0.01 && g<0.2) {mag_Hedges<-c("Muito pequeno")} 
if (g>=0.2 && g<0.5) {mag_Hedges<-c("Pequeno")}
if (g>=0.5 && g<0.8) {mag_Hedges<-c("Intermediario")}
if (g>=0.8 && g<1.2) {mag_Hedges<-c("Grande")}
if (g>=1.2 && g<2) {mag_Hedges<-c("Muito grande")} 
if (g>=2) {mag_Hedges<-c("Enorme")}

# selecao de modelo
R2aj <- (F-1)/((F-1)+df+1)
omega2 <- (F-1)/((F-1)+df+2)

cat("Analise de significancia pratica: tamanho de efeito\n")
cat("\td de Cohen = ",d," (",mag_Cohen,")","\n",sep="")
cat("\tg de Hedges = ",g," (",mag_Hedges,")","\n",sep="")
cat("\teta^2 = R^2 = ",eta2," (",mag_eta2,")\n",sep="")

cat("\nSelecao de modelo:\n")
cat("\tR^2 ajustado =",R2aj,"\n")
cat("\tomega^2 = ", omega2,"\n")

if (!is.na(filename))
{
  cat ("\n-----------------------------------------\n")
  cat("\nVerifique também os graficos:\n")
  for (g in 1:filenum)
  {
    cat ("- ",filegrf[g],"\n")
  }
  cat (  "-----------------------------------------\n")
  sink()
  cat ("\nRelatorio disponivel em ",filetxt,"\n")
} else
{
  cat("\nVerifique também os graficos\n")
}

# deixa a saida grafica no padrao
par(mfrow = c(1, 1))

# enable warnings
options(warn=0)

