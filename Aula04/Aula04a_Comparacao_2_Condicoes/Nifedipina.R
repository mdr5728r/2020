# Teste t para uma condicao, unilateral a direita

# Exemplo:
# 
# Suspeita-se de que um medicamento vasodilatador (Nifedipina)
# para Hipertensao Arterial, amplamente receitado, esteja 
# aumentando a frequencia cardíaca dos pacientes.
# 
# Sabe-se que a frequencia cardiaca na populacao normal 
# tem Distribuicao Normal com media de 70 bpm.
# 
# Para verificar essa suspeita, planejou-se obter uma 
# amostra aleatoria de 50 pacientes que recebem Nifedipina
# para se medir a frequencia cardiaca.

# suppress warnings
options(warn=-1)

# H0: mu=70 (o valor conhecido da populacao ... 70 bpm)
# H1: mu>70 (a suspeita de que ha aumento dos bpm com nifedipina)
mu_pop = 70 # media populacional
alfa <- 0.05 # nivel de significancia adotado

# nome do arquivo de saída (se NA, saida em tela)
cat ("Voce pode guardar os resultados em um arquivo\n") 
cat ("(deixando em branco, a saida eh feita na tela)\n") 
filename <- readline(prompt="Nome do arquivo: ")
if (filename == "") {filename <- NA}

Tabela <- ("
           Paciente   BPM
           1          72
           2          74
           3          70
           4          70
           5          69
           6          71
           7          72
           8          71
           9          69
           10         74
           11         71
           12         71
           13         70
           14         73
           15         69
           16         68
           17         68
           18         71
           19         71
           20         72
           21         70
           22         69
           23         73
           24         69
           25         71
           26         70
           27         72
           28         73
           29         70
           30         72
           31         67
           32         72
           33         67
           34         68
           35         69
           36         72
           37         70
           38         70
           39         70
           40         71
           41         74
           42         67
           43         69
           44         71
           45         71
           46         73
           47         71
           48         71
           49         70
           50         71
")
Nifedipina <- read.table(textConnection(Tabela),header=TRUE)
# a coluna paciente nao deve ser tratada como numero
Nifedipina$Paciente <- as.factor(Nifedipina$Paciente)

# teste t
t_out <- t.test(Nifedipina$BPM, mu=mu_pop, 
                conf.level = 1-alfa, alternative = "greater")

# Referencial teorico da distribuicao t central e nao-central
# Teste t unilateral a direita

df <- t_out$parameter # graus de liberdade
t <- t_out$statistic # estatistica de teste t observada
ncp <- t # ncp = estatistica de teste t
# tamanho de efeito d de Cohen
dp <- sd(Nifedipina$BPM)
m <- t_out$estimate
d <- abs((m-mu_pop)/dp)
# Sawilowsky, S (2009) New effect size rules of thumb. Journal of Modern Applied Statistical Methods 8(2): 467-74.
if (d<0.01) {mag<-c("Desprezivel")} 
if (d>=0.01 && d<0.2) {mag<-c("Muito pequeno")} 
if (d>=0.2 && d<0.5) {mag<-c("Pequeno")}
if (d>=0.5 && d<0.8) {mag<-c("Intermediario")}
if (d>=0.8 && d<1.2) {mag<-c("Grande")}
if (d>=1.2 && d<2) {mag<-c("Muito grande")} 
if (d>=2) {mag<-c("Enorme")}

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

# quantil critico (1-alfa)% sob H0 
if (t_out$alternative == "greater") {
  q <- qt(1-alfa, df)
  poder <- 1-pt(q,df,ncp)
}
if (t_out$alternative == "less") {
  q <- qt(alfa, df) 
  poder <- pt(q,df,ncp)
}

par(mfrow = c(2, 1)) # grid 2 linhas e 1 coluna
if (!is.na(filename))
{
  filepng <- paste(filename,".png", sep="")
  png(filepng, width = 600, height = 400)
}
for (g in 1:2)
{
  if (g == 1)
  {
    plot(dtH0,
         main=paste("Teste t unilateral a direita\ndf =",df,"t =",round(t,5),"alfa =",alfa),
         xlab="t", 
         xlim=c(min(dtH0$x,dtH1$x),max(dtH0$x,dtH1$x)),
         lwd=1, lty=1
    )
    # area do valor p
    abline(v=t,lwd=1,lty=2)
    polx <- dtH0$x[dtH0$x>=t]; polx <- c(min(polx),polx,max(polx))
    poly <- dtH0$y[dtH0$x>=t]; poly <- c(0,poly,0)
    polygon(polx,poly,border="#EE802622",col="#EE802688",lwd=5)
    # t critico
    abline(v=q, lty = 3)
    # legenda
    legend ("topright",
            c("H0","t obs.","t crit.","p"), 
            lwd=c(1,1,1,10,10),
            lty=c(1,2,3,1,1),
            pch=NA,
            col=c("black","black","black","#EE802688"),
            box.lwd=0, bg="transparent")
  }
  if (g == 2)
  {
    plot(dtH0,
         main=NA,
         xlab="t", 
         xlim=c(min(dtH0$x,dtH1$x),max(dtH0$x,dtH1$x)),
         lwd=1, lty=1
    )
    lines(dtH1,lwd=3,lty=1)
    # area alfa
    abline(v=q, lty = 3)
    polx <- dtH0$x[dtH0$x>=q]; polx <- c(min(polx),polx,max(polx))
    poly <- dtH0$y[dtH0$x>=q]; poly <- c(0,poly,0)
    polygon(polx,poly,border="#a3261b22",col="#a3261b88",lwd=5)
    # area beta
    polx <- dtH1$x[dtH1$x<=q]; polx <- c(min(polx),polx,max(polx))
    poly <- dtH1$y[dtH1$x<=q]; poly <- c(0,poly,0)
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
par(mfrow = c(2, 1)) # grid 2 linhas e 1 coluna

if (!is.na(filename))
{
  filetxt <- paste(filename,".txt", sep="")
  sink (filetxt)
  cat ("---------------------------------------\n")
  cat("Processando:",filename,"\n")
  cat ("---------------------------------------\n\n")
} 

# sumarizando BPM
cat(names(Nifedipina)[2],":\n")
sumario <- summary(Nifedipina$BPM)
print (sumario)
cat("\tDesvio-padrao = ",round(dp,2),"\n", sep="")
cat("\tTamanho da amostra = ",length(Nifedipina$BPM),"\n")

# resultado do t.test
print (t_out)

cat("Referencial teorico:\n")
if (!is.na(filename))
{
  cat("\tverifique o grafico no arquivo ", filepng,"\n",sep="")
} else
{
  cat("\tverifique o grafico (no RStudio aparece em Plots)\n",sep="")
}
cat("\tvalor-p =",t_out$p.value,"\n")
cat("\td de Cohen = ",d," (",mag,")","\n\n",sep="")
cat("sob H0: distribuicao t com media = ",mediaH0," e d.p. = ",dpH0,"\n",sep="")
cat("sob H1: distribuicao t com media = ",mediaH1," e d.p. = ",dpH1,"\n",sep="")

if (!is.na(filename))
{
  sink()
  cat ("\n-----------------------------------------\n")
  cat ("Relatorio disponivel em ",filetxt,"\n",sep="")
  cat ("Grafico disponivel em ",filepng,"\n",sep="")
  cat (  "-----------------------------------------\n")
}

# enable warnings
options(warn=0)
