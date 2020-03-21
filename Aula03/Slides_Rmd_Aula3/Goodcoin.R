# Simulacao
# rodando este script np RStudio, observe em Plots

# suppress warnings
options(warn=-1)

cat ("\nDado um valor a receber em moedas de R$1.00, metade da quantia\n") 
cat ("eh oferecida em moedas com um balanceamento de referencia,\n") 
cat ("e metade em moedas falsas, conhecidas por balanceamento distinto.\n") 
cat ("Seu desafio eh distinguir os dois conjuntos atraves de experimentos.\n") 
valor_a_receber <- readline(prompt="Numero de moedas (inteiro, default=10000): ")
if (valor_a_receber=="") {valor_a_receber <- 10000}
valor_a_receber <- as.integer(valor_a_receber)

cat ("\nPara testar se a moeda eh verdadeira, joga-se\n") 
cat ("cara ou coroa certo numero de vezes cada moeda (um experimento).\n") 
jogadasporvez <- readline(prompt="Numero de lancamentos por experimento (numero inteiro, default=15): ")
if (jogadasporvez=="") {jogadasporvez <- 15}
jogadasporvez <- as.integer(jogadasporvez)

cat ("\nQual a proporcao maxima de moedas verdadeiras\n")  
cat ("que voce aceita perder, i.e. alfa = probabilidade do\n")
cat ("erro do tipo I ou de falso-positivo).\n") 
cat ("(numero entre 0 e 1).\n") 
alfa <-readline(prompt="alfa (default=0.05): ")
if (alfa=="") {alfa <- 0.05}
alfa <- as.numeric(alfa)

# para sair coroa H1
cat ("\nAs moedas verdadeiras tem balanceamento de referencia (H0).\n")  
cat ("(caso queira moedas balanceadas, escolha o valor igual a 0.5)\n")
cat ("Qual a probabilidade de sortear coroa para uma moeda verdadeira?\n")  
cat ("(número entre 0 e 1).\n") 
prob_coroa_H0 <-readline(prompt="P[coroa|H0] (default=0.5): ")
if (prob_coroa_H0=="") {prob_coroa_H0 <- 0.5}
prob_coroa_H0 <- as.numeric(prob_coroa_H0)

# para sair coroa H1
cat ("\nAs moedas falsas tem outro balanceamento.\n")  
cat ("(para simular, forneça uma probabilidade diferente de",prob_coroa_H0,"\n")
cat (" ou deixe em branco para simular somente a moeda verdadeira)\n")
cat ("Qual a probabilidade de sortear coroa para uma moeda falsa?\n")  
cat ("(número entre 0 e 1).\n") 
prob_coroa_H1 <-readline(prompt=paste("P[coroa|H1] (default=",prob_coroa_H0,"): ",sep=""))
if (prob_coroa_H1=="") {prob_coroa_H1 <- prob_coroa_H0}
prob_coroa_H1 <- as.numeric(prob_coroa_H1)

# define o valor de alfa efetivo 
# (o maximo que nao ultrapasse
# o valor de alfa solicitado acima)
limiarinf <- -1
limiarsup <- jogadasporvez+1
dt_contagem <- data.frame(0:jogadasporvez,0,0)
names(dt_contagem) <- c("coroas","verdadeira","falsa")
alfa2 <- alfa
alfa_teorico <- 0
if (prob_coroa_H1 == prob_coroa_H0 )
{
  alfa2 <- alfa/2
}
d_prob <- dbinom(0:jogadasporvez,jogadasporvez,prob_coroa_H0)
# upper tail
if (prob_coroa_H1 > prob_coroa_H0 | prob_coroa_H1 == prob_coroa_H0)
{
  i <- jogadasporvez+1
  while(sum(d_prob[i:(jogadasporvez+1)]) < alfa2 ){i <- i-1}
  i <- i+1
  limiarsup <- i-1
  alfa_teorico <- alfa_teorico + sum(d_prob[i:(jogadasporvez+1)])
  # media.maiorint <- ceiling(jogadasporvez*prob_coroa_H0)
  # testebin <- binom.test(media.maiorint, jogadasporvez, prob_coroa_H0,
  #                        alternative="less", conf.level=1-alfa2)
  # limiarsup <- round(testebin$conf.int[2]*jogadasporvez)
  # alfa_teorico <- alfa_teorico + 1-pbinom(limiarsup,jogadasporvez,prob_coroa_H0)
  if (prob_coroa_H1 != prob_coroa_H0)
  {
    limiarinfgrf <- limiarinf-0.5
  }
  limiarsupgrf <- limiarsup-0.5
}
# lower tail
if (prob_coroa_H1 < prob_coroa_H0 | prob_coroa_H1 == prob_coroa_H0)
{
  i <- 1
  while(sum(d_prob[1:i]) < alfa2 ){i <- i+1}
  i <- i-1
  limiarinf <- i-1
  alfa_teorico <- alfa_teorico + sum(d_prob[1:i])
  # media.menorint <- floor(jogadasporvez*prob_coroa_H0)
  # testebin <- binom.test(media.menorint, jogadasporvez, prob_coroa_H0,
  #                        alternative="greater", conf.level=1-alfa2)
  # limiarinf <- round(testebin$conf.int[1]*jogadasporvez)
  # alfa_teorico <- alfa_teorico + pbinom(limiarinf,jogadasporvez,prob_coroa_H0)
  limiarinfgrf <- limiarinf+0.5
  if (prob_coroa_H1 != prob_coroa_H0)
  {
    limiarsupgrf <- limiarsup+0.5
  }
}
alfa_teorico <- round(alfa_teorico*100,1)

prob_acm <- limiarinf+limiarsup

cat ("\nRegra decisoria:\n") 

cat ("Valor para alfa mais próximo do alfa solicitado = ", alfa_teorico,"%\n")

cat ("Em",jogadasporvez,"lancamentos:\n")
if (prob_coroa_H1 == prob_coroa_H0)
{
  cat ("Rejeitar H0 se no. de coroas <= ",limiarinf," ou no. de coroas >= ",limiarsup,"\n")
  cat ("Nao rejeitar H0 se no. de coroas >= ",limiarinf+1," e coroas <= ",limiarsup-1,"\n")
} else
{
  if (prob_coroa_H1 > prob_coroa_H0) # H0 a esquerda
  {
    cat ("Rejeitar H0 se no. de coroas >= ",limiarsup,"\n")
    cat ("Nao rejeitar H0 se no. de coroas <= ",limiarsup-1,"\n")
  }
  if (prob_coroa_H1 < prob_coroa_H0) # H0 a direita
  {
    cat ("Rejeitar H0 se no. de coroas <= ",limiarinf,"\n")
    cat ("Nao rejeitar H0 se no. de coroas >= ",limiarinf+1,"\n")
  }
}
cat ("(este eh o Intervalo de Confianca",round(100-alfa_teorico,2),"% centrado em H0: p[H0]=",prob_coroa_H0,")\n")

cat ("\nIniciando a simulacao\n") 

# limpando as jogadas anteriores
jogadaboa <- rbinom(1, jogadasporvez, prob_coroa_H0)  
moedaboa  <- c(jogadaboa)
divide <- 1
if (prob_coroa_H1 != prob_coroa_H0)
{
 jogadaruim <- rbinom(1, jogadasporvez, prob_coroa_H1)   
 moedaruim <- c(jogadaruim)
 divide <- 2
}
# com que frequencia exibe o grafico (100 graficos)
exibegrf <- valor_a_receber/divide/100; 


# loop, testando as moedas
moedasboasfora <- 0
moedasboasdentro <- 0
if (prob_coroa_H1 != prob_coroa_H0)
{
  moedasruinsfora <- 0
  moedasruinsdentro <- 0
}

hachura <- 500/jogadasporvez
if (hachura < 10) {hachura <- 10}
if (hachura > 35) {hachura <- 35}

for (i in 1:(valor_a_receber/divide))
{
  # jogada de moeda boa
  jogadaboa <- rbinom(1, jogadasporvez, prob_coroa_H0)  
  # cat ("boa: ",jogadaboa,"\n")
  if (jogadaboa <= limiarinf || jogadaboa >= limiarsup) {moedasboasfora <- moedasboasfora+1}
  else {moedasboasdentro <- moedasboasdentro+1}
  if (prob_coroa_H1 != prob_coroa_H0)
  {
    # jogada de moeda desbalanceada
    jogadaruim <- rbinom(1, jogadasporvez, prob_coroa_H1)  
    # cat ("ruim: ",jogadaruim,"\n")
    if (jogadaruim > limiarinf && jogadaruim < limiarsup) {moedasruinsdentro <- moedasruinsdentro+1}
    else {moedasruinsfora <- moedasruinsfora+1}
  }  
  moedaboa  <- c(moedaboa,jogadaboa)
  if (prob_coroa_H1 != prob_coroa_H0)
  {
    moedaruim <- c(moedaruim,jogadaruim)
  }
  erro_1 = round(moedasboasfora/(moedasboasfora+moedasboasdentro)*100, digits=2)
  if (prob_coroa_H1 != prob_coroa_H0)
  {
    erro_2 = round(moedasruinsdentro/(moedasruinsdentro+moedasruinsfora)*100, digits=2)
  }
  
  # exibe 50 graficos ao longo da simulacao, 
  # as primeiras 100 moedas, para mostrar o inicio
  # e a rodada final, quando terminou
  if  ( (i %% exibegrf) == 0 || i <= 100 || i == (valor_a_receber/divide) )
  {
    if (prob_coroa_H1 != prob_coroa_H0)
    {
      titulo <- paste("Aceitou R$",moedasboasdentro," bons e R$", moedasruinsdentro, " falsos (beta=",erro_2,"%), ",jogadasporvez," lances/moeda", sep="")
      subtitulo <- paste("Jogou fora: R$",moedasboasfora," bons (alfa=",erro_1,"%) e R$", moedasruinsfora, " falsos, alfa (teoria)=",alfa_teorico,"%", sep="")
    }
    else
    {
      titulo <- paste("Aceitou R$",moedasboasdentro," ",jogadasporvez," lances/moeda", sep="")
      subtitulo <- paste("Jogou fora R$",moedasboasfora," (alfa=",erro_1,"%), alfa (teoria)=",alfa_teorico,"%", sep="")
    }
    
    moedaboa_table <- table(moedaboa)
    numcoroas_b <- names(table(moedaboa))
    numcoroas_b <- as.numeric(numcoroas_b)-0.15
    ocorrencias_b <- as.numeric(paste(table(moedaboa)))
    if (prob_coroa_H1 != prob_coroa_H0)
    {
      moedaboa_ruim <- table(moedaruim)
      numcoroas_r <- names(table(moedaruim))
      numcoroas_r <- as.numeric(numcoroas_r)+0.15
      ocorrencias_r <- as.numeric(paste(table(moedaruim)))
    }
    else
    {
      ocorrencias_r <- c()
    }
    maxy = max(c(ocorrencias_b,ocorrencias_r))
    plot (numcoroas_b, ocorrencias_b, xlab="coroas (sucessos)", ylab="Contagem", 
          main=titulo, cex.main=0.9, sub=subtitulo, 
          lwd=3, col="#1965B0", axes = FALSE, 
          xlim=c(0,jogadasporvez), ylim=c(0,maxy), type = "h")
    posleg <<- ""
    if (prob_coroa_H1 <= 0.5) {posleg <<- "topright"} else {posleg <<- "topleft"}
    txtH0 <- paste("p[H0]=",prob_coroa_H0,sep="")
    txtH1 <- paste("p[H1]=",prob_coroa_H1,sep="")
    if(prob_coroa_H1 == prob_coroa_H0)
    {
      legend (posleg,c(txtH0,"alfa"), 
              lwd=c(3,15), 
              col=c("#1965B0","#1965B055"), 
              box.lwd=0)
    } else
    {
      legend (posleg,c(txtH0,txtH1,"alfa","beta"), 
              lwd=c(3,3,15,15), 
              col=c("#1965B0","#ac4d12","#1965B055","#ac4d1255"), 
              box.lwd=0)
    }
    ticks <- seq(0:jogadasporvez)
    ticks <- ticks - 1
    axis(side = 1, at = ticks)
    axis(side = 2)  
    if (prob_coroa_H1 != prob_coroa_H0)
    {
      lines(numcoroas_r, ocorrencias_r, lwd=3, col="#ac4d12", type="h")
    }
    lines(c(limiarinfgrf,limiarinfgrf),c(0,maxy),lwd=1,lty=2)
    lines(c(limiarsupgrf,limiarsupgrf),c(0,maxy),lwd=1,lty=2)
    
    # pontos Ho
    points(numcoroas_b[numcoroas_b>limiarinfgrf & numcoroas_b<limiarsupgrf], 
           ocorrencias_b[numcoroas_b>limiarinfgrf & numcoroas_b<limiarsupgrf], 
           col="#1965B0", bg="#1965B0", pch=21)
    if (prob_coroa_H1 != prob_coroa_H0)
    {
      # erro II
      points(numcoroas_r[numcoroas_r>limiarinfgrf & numcoroas_r<limiarsupgrf], 
           ocorrencias_r[numcoroas_r>limiarinfgrf & numcoroas_r<limiarsupgrf], 
           col="#ac4d12", bg="#ac4d12", pch=21)
      lines (numcoroas_r[numcoroas_r>limiarinfgrf & numcoroas_r<limiarsupgrf], 
             ocorrencias_r[numcoroas_r>limiarinfgrf & numcoroas_r<limiarsupgrf], 
             col="#ac4d1255", bg="#ac4d1255", lwd=hachura, type="h")
      # nao rejeita Ho erroneamente
#      x <- numcoroas_r[numcoroas_r>limiarinfgrf & numcoroas_r<limiarsupgrf]
#      y <- ocorrencias_r[numcoroas_r>limiarinfgrf & numcoroas_r<limiarsupgrf]
#      x <- c(min(x), x, max(x))
#      y <- c(0, y, 0)
#      polygon(x, y, col="#ac4d1288", border="#ac4d1288")    
      # pontos Ha
    }
    # erro I
    points(numcoroas_b[numcoroas_b<limiarinfgrf | numcoroas_b>limiarsupgrf], 
           ocorrencias_b[numcoroas_b<limiarinfgrf | numcoroas_b>limiarsupgrf], 
           col="#1965B0", bg="#1965B0", pch=4)
    lines(numcoroas_b[numcoroas_b<limiarinfgrf | numcoroas_b>limiarsupgrf], 
           ocorrencias_b[numcoroas_b<limiarinfgrf | numcoroas_b>limiarsupgrf], 
           col="#1965B055", bg="#1965B055", lwd=hachura, type="h")
    # cauda inferior
#    x <- numcoroas_b[numcoroas_b<limiarinfgrf]
#    y <- ocorrencias_b[numcoroas_b<limiarinfgrf]
#    x <- c(min(x), x, max(x))
#    y <- c(0, y, 0)
#    polygon(x, y, col="#1965B088", border="#1965B088")    
    # cauda superior
#    x <- numcoroas_b[numcoroas_b>limiarsupgrf]
#    y <- ocorrencias_b[numcoroas_b>limiarsupgrf]
#    x <- c(min(x), x, max(x))
#    y <- c(0, y, 0)
#    polygon(x, y, col="#1965B088", border="#1965B088")    
    if (prob_coroa_H1 != prob_coroa_H0)
    {
      points(numcoroas_r[numcoroas_r<limiarinfgrf | numcoroas_r>limiarsupgrf], 
           ocorrencias_r[numcoroas_r<limiarinfgrf | numcoroas_r>limiarsupgrf], 
           col="#ac4d12", bg="#ac4d12", pch=4)
    }
    # pausa para ver o grafico
    Sys.sleep(0.2)
  }
}

cat ("\nTerminado\n")

# exibe a contagem das moedas
cat ("Distribuicoes\n")
cat ("- moedas verdadeiras:\n")
ocorrencias_v <- table(moedaboa)
print (ocorrencias_v)
if (prob_coroa_H1 != prob_coroa_H0)
{
  cat ("- moedas falsas:\n")
  ocorrencias_f <- table(moedaruim)
  print (ocorrencias_f)
}

cat ("\n*** Regras Decisorias ***\n")
cat ("Em",jogadasporvez,"lancamentos:\n")
if (prob_coroa_H1 == prob_coroa_H0)
{
  cat ("Rejeitar H0 se no. de coroas <= ",limiarinf," ou no. de coroas >= ",limiarsup,"\n")
} else
{
  if (prob_coroa_H1 > prob_coroa_H0) # H0 a esquerda
  {
    cat ("Rejeitar H0 se no. de coroas >= ",limiarsup,"\n")
  }
  if (prob_coroa_H1 < prob_coroa_H0) # H0 a direita
  {
    cat ("Rejeitar H0 se no. de coroas <= ",limiarinf,"\n")
  }
}
poder <- 0
if (prob_coroa_H1 != prob_coroa_H0)
{
  poder <- 100-erro_2
  cat ("Poder=",poder,"%:\n")
}
if (poder >= 90)
{
  sufixo <- "Aceitar"
} else # falta poder
{
  sufixo <- "Nao rejeitar"
}
if (prob_coroa_H1 == prob_coroa_H0)
{
  cat (sufixo,"H0 se no. de coroas >= ",limiarinf+1," e coroas <= ",limiarsup-1,"\n")
} else
{
  if (prob_coroa_H1 > prob_coroa_H0) # H0 a esquerda
  {
    cat (sufixo,"H0 se no. de coroas <= ",limiarsup-1,"\n")
  }
  if (prob_coroa_H1 < prob_coroa_H0) # H0 a direita
  {
    cat (sufixo,"H0 se no. de coroas >= ",limiarinf+1,"\n")
  }
}
cat ("(este eh o Intervalo de Confianca",round(100-alfa_teorico,2),"% centrado em H0: p[H0]=",prob_coroa_H0,")\n")

# enable warnings
options(warn=0)
