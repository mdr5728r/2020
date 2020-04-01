# Teste t para duas condicoes dependentes, unilateral a esquerda

# Exemplo:
# 
# A professora Yob esta interessada na violencia de massa durante as 
# partidas de futebol. Ela pensa que a violencia do grupo eh resultado 
# dos assentos desconfortaveis do estádio. Por isso, Yob modifica dois
# estadios diferentes na Inglaterra. Em um estadio coloca assentos bem 
# apertados e desconfortaveis. No outro, instala assentos confortaveis, 
# com muito espaço para as pernas e entre os assentos adjacentes. 
# A professora organiza uma competicao, de modo que um clube jogue 
# metade das partidas em um estadio e a outra metade no outro estadio. 
# Ela acompanha um grupo de 12 fas adolescentes agressivos e grosseiros 
# do clube e registra o numero de vezes que cada um eh preso ou expulso 
# do estadio. Preve que o numero de prisoes e expulsoes sera menor no 
# estadio que apresenta os assentos mais confortaveis.

# suppress warnings
options(warn=-1)

# H0: mu_conforto - mu_desconforto = 0 
# H1: mu_conforto - mu_desconforto < 0 
alfa <- 0.05 # nivel de significancia adotado
# nome do arquivo de saída (se NA, saida em tela)
cat ("Voce pode guardar os resultados em um arquivo\n") 
cat ("(deixando em branco, a saida eh feita na tela)\n") 
filename <- readline(prompt="Nome do arquivo: ")
if (filename == "") {filename <- NA}

# define os nomes das variaveis 
vi_name <- "Estadio"
vd_name <- "Ocorrencias"

Tabela <- ("
Adolescente Desconforto Conforto
a           8           3
b           5           2
c           4           4
d           6           6
e           4           2
f           8           1
g           9           6
h           10          3
i           7           4
j           8           1
k           6           4
l           7           3
")
Dtfrm <- read.table(textConnection(Tabela),header=TRUE)
# diferenca entre a terceira e a segunda coluna
dif <- Dtfrm[[3]]-Dtfrm[[2]]

# Graficos (estatistica descritiva)
filegrf <- c()
filenum <- 0
# boxplot de cada conjunto de dados
if (!is.na(filename))
{
  filenum <- filenum+1
  filegrf <- c(filegrf, paste(filename,"_boxplot.png", sep="")); 
  png(filegrf[filenum], width = 600, height = 400)
}
boxplot (unlist(Dtfrm[2]), unlist(Dtfrm[3]), 
         names= c(names(Dtfrm)[2],names(Dtfrm)[3]),
         main="Distribuicao dos dados",xlab=vi_name,ylab=vd_name)
if (!is.na(filename))
{
  dev.off()
}
# boxplot da diferenca  entre os dois conjuntos de dados
if (!is.na(filename))
{
  filenum <- filenum+1
  filegrf <- c(filegrf, paste(filename,"_boxplot_dif.png", sep="")); 
  png(filegrf[filenum], width = 600, height = 400)
}
boxplot (dif, 
         main=paste("Diferencas de ocorrencias (",names(Dtfrm)[3],"-",names(Dtfrm)[2],")",sep=""),
         xlab="", ylab=paste (vd_name," (diferenca)",sep=""))
if (!is.na(filename))
{
  dev.off()
}
# density plot
if (!is.na(filename))
{
  filenum <- filenum+1
  filegrf <- c(filegrf, paste(filename,"_densplot.png", sep="")); 
  png(filegrf[filenum], width = 600, height = 400)
}
densidade1 <- density(unlist(Dtfrm[2]))
densidade2 <- density(unlist(Dtfrm[3]))
plot (densidade1, main="Distribuicao dos dados",
      xlab=vd_name,ylab="Densidade",
      xlim = c(min(densidade1$x,densidade2$x),max(densidade1$x,densidade2$x)),
      ylim = c(min(densidade1$y,densidade2$y),max(densidade1$y,densidade2$y))
      )
lines (densidade2, lty=2)
legend ("topright",c(names(Dtfrm)[2],names(Dtfrm)[3]),lty=c(1,2))
if (!is.na(filename))
{
  dev.off()
}
# density plot das diferencas
if (!is.na(filename))
{
  filenum <- filenum+1
  filegrf <- c(filegrf, paste(filename,"_densplot_dif.png", sep="")); 
  png(filegrf[filenum], width = 600, height = 400)
}
densidade <- density(dif)
plot (densidade, main="Distribuicao dos dados",
      xlab=paste(vd_name," (",names(Dtfrm)[3],"-",names(Dtfrm)[2],")", sep=""),ylab="Densidade"
)
if (!is.na(filename))
{
  dev.off()
}

# coeficiente de correlacao de Pearson
corr_test <- cor.test(Dtfrm$Desconforto,Dtfrm$Conforto)

# significancia estatistica
t_out <- t.test(dif,mu=0,alternative="less")
t <- t_out$statistic
F <- t^2
df <- t_out$parameter
eta2 <- F/(F+df)
f2 <- eta2/(1-eta2)
# Elis P (2010) The essential guide to effect sizes. Cambrige 
if (eta2 <0.01) {mag_eta2<-c("Desprezivel")} 
if (eta2>=0.01 && eta2<0.06) {mag_eta2<-c("Pequeno")} 
if (eta2>=0.06 && eta2<0.14) {mag_eta2<-c("Intermediario")}
if (eta2>=0.14) {mag_eta2<-c("Grande")}

ncp <- t
if (t_out$alternative == "greater")
{
  q <- qt(1-alfa, df)
  power <- 1 - pt(q,df,ncp)
}
if (t_out$alternative == "less")
{
  q <- qt(alfa, df)
  power <- pt(q,df,ncp)
}
if (t_out$alternative == "two.sided")
{
  q <- c(qt(alfa/2,df), qt(alfa/2,df,lower.tail=FALSE))
  power <- pt(q[1],df,ncp)+(1-pt(q[2],df,ncp))
}
R2aj <- (F-1)/(F+df)

# tamanho de efeito d de Cohen
dp <- sd(dif)
m <- t_out$estimate
d <- abs(t_out$statistic)/sqrt(t_out$parameter+1)
# Sawilowsky, S (2009) New effect size rules of thumb. Journal of Modern Applied Statistical Methods 8(2): 467-74.
if (d<0.01) {mag_Cohen<-c("Desprezivel")} 
if (d>=0.01 && d<0.2) {mag_Cohen<-c("Muito pequeno")} 
if (d>=0.2 && d<0.5) {mag_Cohen<-c("Pequeno")}
if (d>=0.5 && d<0.8) {mag_Cohen<-c("Intermediario")}
if (d>=0.8 && d<1.2) {mag_Cohen<-c("Grande")}
if (d>=1.2 && d<2) {mag_Cohen<-c("Muito grande")} 
if (d>=2) {mag_Cohen<-c("Enorme")}

# media e dp das dist. t central e nao-central
mediaH0 <- 0
dpH0 <- sqrt(df/(df-2))
beta <- sqrt(df/2)*gamma((df-1)/2)/gamma(df/2)
mediaH1 <- ncp*beta
dpH1 <- sqrt((df*(1+ncp^2)/(df-2))-mediaH1^2)

# distribuicao t sob H0 (central: ncp = 0)
tH0 <- rt(1e6, df)
dtH0 <- density(tH0)

# distribuicao t sob H1, ncp = t
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

# sumarizando 
cat("\nEsquema de 5 estatisticas de Tukey & media\n")

cat("\nTamanho da amostra: ",length(dif),"\n", sep="")

cat("\nNumero de ocorrencias em ",names(Dtfrm)[2],":\n", sep="")
sumario <- summary(Dtfrm[[2]], digits = 3)
print (sumario)
cat("Numero de ocorrencias em ",names(Dtfrm)[3],":\n", sep="")
sumario <- summary(Dtfrm[[3]], digits = 3)
print (sumario)
cat("Diferenca do numero de ocorrencias (",names(Dtfrm)[3]," - ",names(Dtfrm)[2],"):\n", sep="")
sumario <- summary(dif, digits = 3)
print (sumario)

cat("\nCoeficiente de correlacao de Pearson\n")
print (corr_test)
cat("Analise de significancia estatistica: valor-p\n")
print(t_out)

# Grafico
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
         main=paste("Teste t unilateral a esquerda\ndf =",df,"t =",round(t,5),"alfa =",alfa),
         xlab="t", 
         xlim=c(min_x,max_x),
         lwd=1, lty=1
    )
    # area do valor p
    abline(v=t,lwd=1,lty=2)

    if (t_out$alternative == "greater")
    {
      polx <- dtH0$x[dtH0$x>=t]; polx <- c(min(polx),polx,max(polx))
      poly <- dtH0$y[dtH0$x>=t]; poly <- c(0,poly,0)
    }
    if (t_out$alternative == "less")
    {
      polx <- dtH0$x[dtH0$x<=t]; polx <- c(min(polx),polx,max(polx))
      poly <- dtH0$y[dtH0$x<=t]; poly <- c(0,poly,0)
    }
    if (t_out$alternative == "two.sided")
    {
      polx <- dtH0$x[dtH0$x>=t]; 
      polx <- dtH0$x[dtH0$x<=t]; polx <- c(min(polx),polx,max(polx))
      poly <- dtH0$y[dtH0$x>=t]; 
      poly <- dtH0$y[dtH0$x<=t]; poly <- c(0,poly,0)
    }
    polygon(polx,poly,border="#EE802622",col="#EE802688",lwd=5)
    # t critico
    abline(v=q, lty = 3)
    # legenda
    legend ("topright",
            c("H0","t obs.","t crit.","p"), 
            lwd=c(1,1,1,10),
            lty=c(1,2,3,1),
            pch=NA,
            col=c("black","black","black","#EE802688"),
            box.lwd=0, bg="transparent")
  }
  if (g == 2)
  {
    plot(dtH0,
         main=NA,
         xlab="t", 
         xlim=c(min_x,max_x),
         lwd=1, lty=1
    )
    lines(dtH1,lwd=3,lty=1)
    # area alfa
    abline(v=q, lty = 3)
    if (t_out$alternative == "greater")
    {
      polx <- dtH0$x[dtH0$x>=q]; polx <- c(min(polx),polx,max(polx))
      poly <- dtH0$y[dtH0$x>=q]; poly <- c(0,poly,0)
    }
    if (t_out$alternative == "less")
    {
      polx <- dtH0$x[dtH0$x<=q]; polx <- c(min(polx),polx,max(polx))
      poly <- dtH0$y[dtH0$x<=q]; poly <- c(0,poly,0)
    }
    if (t_out$alternative == "two.sided")
    {
      polx <- dtH0$x[dtH0$x>=q]; 
      polx <- dtH0$x[dtH0$x<=q]; polx <- c(min(polx),polx,max(polx))
      poly <- dtH0$y[dtH0$x>=q]; 
      poly <- dtH0$y[dtH0$x<=q]; poly <- c(0,poly,0)
    }
    polygon(polx,poly,border="#a3261b22",col="#a3261b88",lwd=5)
    # area beta
    if (t_out$alternative == "greater")
    {
      polx <- dtH1$x[dtH1$x<=q]; polx <- c(min(polx),polx,max(polx))
      poly <- dtH1$y[dtH1$x<=q]; poly <- c(0,poly,0)
    }
    if (t_out$alternative == "less")
    {
      polx <- dtH1$x[dtH1$x>=q]; polx <- c(min(polx),polx,max(polx))
      poly <- dtH1$y[dtH1$x>=q]; poly <- c(0,poly,0)
    }
    if (t_out$alternative == "two.sided")
    {
      polx <- dtH1$x[dtH1$x<=q]; 
      polx <- dtH1$x[dtH1$x>=q]; polx <- c(min(polx),polx,max(polx))
      poly <- dtH1$y[dtH1$x<=q]; 
      poly <- dtH1$y[dtH1$x>=q]; poly <- c(0,poly,0)
    }
    polygon(polx,poly,border="#4EB26522",col="#4EB26588",lwd=8)
    
    # legenda
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
par(mfrow = c(1, 1)) # restaura grid 1 linha e 1 coluna

cat("\nReferencial teorico:\n")
cat("\tvalor-p =",t_out$p.value,"\n")
cat("Analise de poder retrospectivo:\n")
cat("\tPoder =",power,"\n\n")
cat("sob H0: distribuicao t com media = ",mediaH0," e d.p. = ",dpH0,"\n",sep="")
cat("sob H1: distribuicao t com media = ",mediaH1," e d.p. = ",dpH1,"\n",sep="")
cat("\n")
cat("Analise de significancia pratica: tamanho de efeito\n")
cat("\td de Cohen = ",d," (",mag_Cohen,")","\n",sep="")
cat("\teta^2 = R^2 = ",eta2," (",mag_eta2,")\n",sep="")
cat("\nSelecao de modelo:\n")
cat("\tR^2 ajustado =",R2aj)

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

# enable warnings
options(warn=0)
