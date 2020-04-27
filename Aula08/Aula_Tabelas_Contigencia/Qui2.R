# Simulacao: Qui-quadrado
# rodando este script np RStudio, observe em Plots

# suppress warnings
options(warn=-1)

cat ("**** TABELA DE CONTINGÊNCIA ****\n") 

cat ("\nConsidere:\n") 
output <- data.frame(
  c1 = c("", "Linha +", "Linha -", "" ),
  c2 = c("Coluna+", "a", "c", "Total col. +"),
  c3 = c("Coluna-", "b", "d", "Total col. -"),
  c4 = c("", "Total lin. +", "Total lin. -", "Total geral")
)
names(output) <- NULL
print(output, row.names = FALSE)

cat ("\n\nInforme:") 
nomelin <- readline(prompt="Nome nas linhas (ate 10 letras): ")
nomecol <- readline(prompt="Nome nas colunas (ate 10 letras): ")

cat ("Definido:\n") 
output <- data.frame(
  c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
  c2 = c(paste(nomecol,"+"), "a", "c", paste("Total de",nomelin,"+")),
  c3 = c(paste(nomecol,"-"), "b", "d", paste("Total de",nomelin,"-")),
  c4 = c("", paste("Total de",nomecol,"+"), paste("Total de",nomecol,"-"), "Total geral")
)
names(output) <- NULL
print(output, row.names = FALSE)

cat ("\n") 
cat ("Forneca os valores de a, b, c, d:\n") 

a <- readline(prompt="a:")
a <- as.numeric(a)
if (is.na(a)) {a <- 0}

b <- readline(prompt="b:")
b <- as.numeric(b)
if (is.na(b)) {b <- 0}

c <- readline(prompt="c:")
c <- as.numeric(c)
if (is.na(c)) {c <- 0}

d <- readline(prompt="d:")
d <- as.numeric(d)
if (is.na(d)) {d <- 0}

# caso tenha fornecido em proporcoes
total_abcd = a+b+c+d
if (total_abcd == 1)
{
  cat ("\n") 
  cat ("Qual o tamanho de sua amostra ( = Total geral)?\n") 
  cat ("(entre um numero inteiro; default n=10000\n") 
  total_abcd <- readline(prompt="n: ")
  total_abcd <- as.integer(total_abcd)
  if (is.na(total_abcd)) {total_abcd <- 10000}
  a <- a*total_abcd
  b <- b*total_abcd
  c <- c*total_abcd
  d <- d*total_abcd
}


cat ("\n")  
cat ("Defina alfa = prob. erro do tipo I).\n") 
cat ("(número entre 0 e 1, default = 0.05).\n") 
alfa <-readline(prompt="alfa: ")
alfa <- as.numeric(alfa)
if (is.na(alfa)) {alfa <- 0.05; cat(alfa);}

cat ("\n") 
cat ("Quantas iteracoes para simular?\n") 
cat ("(entre um numero inteiro; default n=1e4)\n") 
num_iteracoes <- readline(prompt="iteracoes: ")
num_iteracoes <- as.integer(num_iteracoes)
if (is.na(num_iteracoes)) {num_iteracoes <- 1e4; cat(num_iteracoes);}

cat ("\n") 
cat ("Exibir tabelas? (exibir lentifica a simulacao)\n") 
exibetabela <- readline(prompt="0=nao, 1=sim; default eh 0: ")
exibetabela <- as.integer(exibetabela)
if (is.na(exibetabela)) {exibetabela <- 0; cat(exibetabela);}


# H1
a_obs <- a/total_abcd
b_obs <- b/total_abcd
c_obs <- c/total_abcd
d_obs <- d/total_abcd

# H0
a_esp <- (a_obs+b_obs)*(a_obs+c_obs)
b_esp <- (a_obs+b_obs)*(b_obs+d_obs)
c_esp <- (a_obs+c_obs)*(c_obs+d_obs)
d_esp <- (c_obs+d_obs)*(b_obs+d_obs)

cat ("\n\nIniciando a simulacao\n") 

cat ("\n\nDefinidos:\n") 
output <- data.frame(
  c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
  c2 = c(paste(nomecol,"+"), round(a,3), round(c,3), round((a+c),3)),
  c3 = c(paste(nomecol,"-"), round(b,3), round(d,3), round((b+d),3)),
  c4 = c("", round(a+b,3), round(c+d,3), round(total_abcd,3))
)
names(output) <- NULL
print(output, row.names = FALSE)

# Exibe H0 e H1
cat ("\n") 
cat ("Precisamos estabelecer se",nomecol,"depende de",nomelin,".\n") 

cat("\n")
cat ("\nH0 (em proporcoes, valores esperados):\n") 
output <- data.frame(
  c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
  c2 = c(paste(nomecol,"+"), round(a_esp,3), round(c_esp,3), round((a_esp+c_esp),3)),
  c3 = c(paste(nomecol,"-"), round(b_esp,3), round(d_esp,3), round((b_esp+d_esp),3)),
  c4 = c("", round(a_esp+b_esp,3), round(c_esp+d_esp,3), 1)
)
names(output) <- NULL
print(output, row.names = FALSE)
cat("\n")
cat ("\nH1 (em proporcoes, valores observados):\n") 
output <- data.frame(
  c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
  c2 = c(paste(nomecol,"+"), round(a_obs,3), round(c_obs,3), round((a_obs+c_obs),3)),
  c3 = c(paste(nomecol,"-"), round(b_obs,3), round(d_obs,3), round((b_obs+d_obs),3)),
  c4 = c("", round(a_obs+b_obs,3), round(c_obs+d_obs,3), 1)
)
names(output) <- NULL
print(output, row.names = FALSE)

cat("\n")
cat("\namostra=",total_abcd)
cat ("\nalfa =",alfa)
cat ("\niteracoes =",num_iteracoes)

# com que frequencia exibe o grafico (100 graficos)
exibegrf <- num_iteracoes/100; 

# valores de referencia
h0_ref <- c(a_esp,b_esp,c_esp,d_esp)
h0_probs <- c()
acm <- 0
for (idx in 1:4)
{
  acm <- acm+h0_ref[idx]
  h0_probs <- c(h0_probs, acm)
}
h1_ref <- c(a_obs,b_obs,c_obs,d_obs)
h1_probs <- c()
acm <- 0
for (idx in 1:4)
{
  acm <- acm+h1_ref[idx]
  h1_probs <- c(h1_probs, acm)
}

# iterando
h0_qui <- c()
h0_p <- c()
h0_OR <- c()
h1_qui <- c()
h1_p <- c()
h1_OR <- c()
h1_VCramer <- c()
for (i in 1:num_iteracoes)
{
  # distribuindo os individuos sob H0  
  aloca_h0 <- c(0,0,0,0)
  for (a in 1:total_abcd)
  {
    rnd=runif(1,min=0,max=1)  
    for (idx in 1:4)
    {
      if (rnd <= h0_probs[idx])
      {
        aloca_h0[idx] <- aloca_h0[idx]+1
        break
      }
    }
  }
  # converte em % ... aloca_h0 na ordem a,b,c,d
  for (idx in 1:4)
  {
    aloca_h0[idx] <- aloca_h0[idx]/total_abcd 
  }
  h0_sml_obs <- c(0,0,0,0)
  h0_sml_obs[1] <- aloca_h0[1]
  h0_sml_obs[2] <- aloca_h0[2]
  h0_sml_obs[3] <- aloca_h0[3]
  h0_sml_obs[4] <- aloca_h0[4]

  h0_sml_esp <- c(0,0,0,0)
  h0_sml_esp[1] <- (aloca_h0[1]+aloca_h0[2])*(aloca_h0[1]+aloca_h0[3])
  h0_sml_esp[2] <- (aloca_h0[1]+aloca_h0[2])*(aloca_h0[2]+aloca_h0[4])
  h0_sml_esp[3] <- (aloca_h0[1]+aloca_h0[3])*(aloca_h0[3]+aloca_h0[4])
  h0_sml_esp[4] <- (aloca_h0[3]+aloca_h0[4])*(aloca_h0[2]+aloca_h0[4])
  phi_h0 <- 0
  for (idx in 1:4)
  {
    phi_h0 <- phi_h0 + ((h0_sml_obs[idx]-h0_sml_esp[idx])^2)/h0_sml_esp[idx]
  }
  qui2_h0 <- phi_h0 * total_abcd
  h0_qui <- c(h0_qui,qui2_h0)
  h0_p <- c(h0_p,1-pchisq(qui2_h0,1))

  OR_h0 <- (aloca_h0[1]*aloca_h0[4])/(aloca_h0[2]*aloca_h0[3])
  if (is.finite(OR_h0))
  {
    h0_OR <- c(h0_OR,OR_h0)
  }
  # distribuindo os individuos sob H1  
  aloca_h1 <- c(0,0,0,0)
  for (a in 1:total_abcd)
  {
    rnd=runif(1,min=0,max=1)  
    for (idx in 1:4)
    {
      if (rnd <= h1_probs[idx])
      {
        aloca_h1[idx] <- aloca_h1[idx]+1
        break
      }
    }
  }
  # converte em % ... aloca_h1 na ordem a,b,c,d
  for (idx in 1:4)
  {
    aloca_h1[idx] <- aloca_h1[idx]/total_abcd 
  }
  h1_sml_obs <- c(0,0,0,0)
  h1_sml_obs[1] <- aloca_h1[1]
  h1_sml_obs[2] <- aloca_h1[2]
  h1_sml_obs[3] <- aloca_h1[3]
  h1_sml_obs[4] <- aloca_h1[4]
  
  h1_sml_esp <- c(0,0,0,0)
  h1_sml_esp[1] <- (aloca_h1[1]+aloca_h1[2])*(aloca_h1[1]+aloca_h1[3])
  h1_sml_esp[2] <- (aloca_h1[1]+aloca_h1[2])*(aloca_h1[2]+aloca_h1[4])
  h1_sml_esp[3] <- (aloca_h1[1]+aloca_h1[3])*(aloca_h1[3]+aloca_h1[4])
  h1_sml_esp[4] <- (aloca_h1[3]+aloca_h1[4])*(aloca_h1[2]+aloca_h1[4])
  phi_h1 <- 0
  for (idx in 1:4)
  {
    phi_h1 <- phi_h1 + ((h1_sml_obs[idx]-h1_sml_esp[idx])^2)/h1_sml_esp[idx]
  }
  qui2_h1 <- phi_h1 * total_abcd
  h1_qui <- c(h1_qui,qui2_h1)
  h1_p <- c(h1_p,1-pchisq(qui2_h1,1))
  
  OR_h1 <- (aloca_h1[1]*aloca_h1[4])/(aloca_h1[2]*aloca_h1[3])
  if (is.finite(OR_h1))
  {
    h1_OR <- c(h1_OR,OR_h1)
  }
  # V de Cramer
  nL <- 2 #nrow(TC) # ou dim(TC)[1]
  nC <- 2 #ncol(TC) # ou dim(TC)[2]
  Dim <- min(nL,nC)-1 # dimensoes da TC: dim = max(phi^2)
  V_h1 <- sqrt(phi_h1/Dim) # V ou C de Cramer
  h1_VCramer <- c(h1_VCramer,V_h1)
  
  if (exibetabela == 1)
  {
    cat("\n")
    cat ("\n----------------------------------------------\n") 
    cat ("\nValores simulados sob H0:\n") 
    cat ("\n- Individuos alocados:") 
    cat ("\n--- observados:") 
    output <- data.frame(
      c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
      c2 = c(paste(nomecol,"+"), round(aloca_h0[1]*total_abcd,0), round(aloca_h0[3]*total_abcd,0), round((aloca_h0[1]+aloca_h0[3])*total_abcd,0)),
      c3 = c(paste(nomecol,"-"), round(aloca_h0[2]*total_abcd,0), round(aloca_h0[4]*total_abcd,0), round((aloca_h0[2]+aloca_h0[4])*total_abcd,0)),
      c4 = c("", round((aloca_h0[1]+aloca_h0[2])*total_abcd,0), round((aloca_h0[3]+aloca_h0[4])*total_abcd,0), total_abcd )
    )
    names(output) <- NULL
    print(output, row.names = FALSE)
    cat ("\n--- esperados:") 
    output <- data.frame(
      c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
      c2 = c(paste(nomecol,"+"), round(h0_sml_esp[1]*total_abcd,3), round(h0_sml_esp[3]*total_abcd,3), round((h0_sml_esp[1]+h0_sml_esp[3])*total_abcd,3)),
      c3 = c(paste(nomecol,"-"), round(h0_sml_esp[2]*total_abcd,3), round(h0_sml_esp[4]*total_abcd,3), round((h0_sml_esp[2]+h0_sml_esp[4])*total_abcd,3)),
      c4 = c("", round((h0_sml_esp[1]+h0_sml_esp[2])*total_abcd,3), round((h0_sml_esp[3]+h0_sml_esp[4])*total_abcd,3), total_abcd)
    )
    names(output) <- NULL
    print(output, row.names = FALSE)
    cat ("\n- Em probabilidades:") 
    cat ("\n--- observados:") 
    output <- data.frame(
      c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
      c2 = c(paste(nomecol,"+"), round(h0_sml_obs[1],3), round(h0_sml_obs[3],3), round((h0_sml_obs[1]+h0_sml_obs[3]),3)),
      c3 = c(paste(nomecol,"-"), round(h0_sml_obs[2],3), round(h0_sml_obs[4],3), round((h0_sml_obs[2]+h0_sml_obs[4]),3)),
      c4 = c("", round(h0_sml_obs[1]+h0_sml_obs[2],3), round(h0_sml_obs[3]+h0_sml_obs[4],3), 1)
    )
    names(output) <- NULL
    print(output, row.names = FALSE)
    cat ("\n--- esperados:") 
    output <- data.frame(
      c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
      c2 = c(paste(nomecol,"+"), round(h0_sml_esp[1],3), round(h0_sml_esp[3],3), round((h0_sml_esp[1]+h0_sml_esp[3]),3)),
      c3 = c(paste(nomecol,"-"), round(h0_sml_esp[2],3), round(h0_sml_esp[4],3), round((h0_sml_esp[2]+h0_sml_esp[4]),3)),
      c4 = c("", round(h0_sml_esp[1]+h0_sml_esp[2],3), round(h0_sml_esp[3]+h0_sml_esp[4],3), 1)
    )
    names(output) <- NULL
    print(output, row.names = FALSE)
    cat ("\nPhi^2 de Cohen:",phi_h0)
    cat ("\nQui-quadrado:",qui2_h0,", p:",round(1-pchisq(qui2_h0,1),5))
    cat ("\nOdds Ratio:",OR_h0,"\n")
    cat("\n")
    cat ("\nValores simulados sob H1:\n") 
    cat ("\n- Individuos alocados:") 
    cat ("\n--- observados:") 
    output <- data.frame(
      c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
      c2 = c(paste(nomecol,"+"), round(aloca_h1[1]*total_abcd,0), round(aloca_h1[3]*total_abcd,0), round((aloca_h1[1]+aloca_h1[3])*total_abcd,0)),
      c3 = c(paste(nomecol,"-"), round(aloca_h1[2]*total_abcd,0), round(aloca_h1[4]*total_abcd,0), round((aloca_h1[2]+aloca_h1[4])*total_abcd,0)),
      c4 = c("", round((aloca_h1[1]+aloca_h1[2])*total_abcd,0), round((aloca_h1[3]+aloca_h1[4])*total_abcd,0), total_abcd )
    )
    names(output) <- NULL
    print(output, row.names = FALSE)
    cat ("\n--- esperados:") 
    output <- data.frame(
      c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
      c2 = c(paste(nomecol,"+"), round(h1_sml_esp[1]*total_abcd,3), round(h1_sml_esp[3]*total_abcd,3), round((h1_sml_esp[1]+h1_sml_esp[3])*total_abcd,3)),
      c3 = c(paste(nomecol,"-"), round(h1_sml_esp[2]*total_abcd,3), round(h1_sml_esp[4]*total_abcd,3), round((h1_sml_esp[2]+h1_sml_esp[4])*total_abcd,3)),
      c4 = c("", round((h1_sml_esp[1]+h1_sml_esp[2])*total_abcd,3), round((h1_sml_esp[3]+h1_sml_esp[4])*total_abcd,3), total_abcd)
    )
    names(output) <- NULL
    print(output, row.names = FALSE)
    cat ("\n- Em probabilidades:") 
    cat ("\n--- observados:") 
    output <- data.frame(
      c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
      c2 = c(paste(nomecol,"+"), round(h1_sml_obs[1],3), round(h1_sml_obs[3],3), round((h1_sml_obs[1]+h1_sml_obs[3]),3)),
      c3 = c(paste(nomecol,"-"), round(h1_sml_obs[2],3), round(h1_sml_obs[4],3), round((h1_sml_obs[2]+h1_sml_obs[4]),3)),
      c4 = c("", round(h1_sml_obs[1]+h1_sml_obs[2],3), round(h1_sml_obs[3]+h1_sml_obs[4],3), 1)
    )
    names(output) <- NULL
    print(output, row.names = FALSE)
    cat ("\n--- esperados:") 
    output <- data.frame(
      c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
      c2 = c(paste(nomecol,"+"), round(h1_sml_esp[1],3), round(h1_sml_esp[3],3), round((h1_sml_esp[1]+h1_sml_esp[3]),3)),
      c3 = c(paste(nomecol,"-"), round(h1_sml_esp[2],3), round(h1_sml_esp[4],3), round((h1_sml_esp[2]+h1_sml_esp[4]),3)),
      c4 = c("", round(h1_sml_esp[1]+h1_sml_esp[2],3), round(h1_sml_esp[3]+h1_sml_esp[4],3), 1)
    )
    names(output) <- NULL
    print(output, row.names = FALSE)
    cat ("\nPhi^2 de Cohen:",phi_h1)
    cat ("\nQui-quadrado:",qui2_h1,", p:",round(1-pchisq(qui2_h1,1),5))
    cat ("\nOdds Ratio:",OR_h1,"\n")
    cat("\n")
    cat ("\nV de Cramer:",V_h1,"\n")
  }  

  if  ( length(h0_qui)>=2 & ((i %% exibegrf) == 0 || (i>=10 & i <= 100)) )
  {
    # densidades
    h0_qui <- sort(h0_qui)
    h1_qui <- sort(h1_qui)
    dens_h0 <- density(h0_qui, na.rm=TRUE)
    dens_h1 <- density(h1_qui, na.rm=TRUE)
    
    # xlim, ylim
    df <- 1

    # quantil 95% (empirico)
    q95 <- quantile(h0_qui,probs=1-alfa, na.rm=TRUE)

    # media da dist. qui^2
    mediah0 <- df
    dph0 <- sqrt(2*df)
    mediah1 <- mean(h1_qui)
    dph1 <- sqrt(2*df+mediah1)
    # sugestao para os eixos, evitando cauda muito longa
    truncar_x <- 0.05
    maxx <- mediah1+3*dph1
    maxy <- max(dens_h0$y, dens_h1$y)
    
    cor_H0 = "#1965B0"
    cor_alfa_transparencia = paste(cor_H0,"88",sep="")
    cor_H1 = "#a30b1b"
    cor_beta_transparencia = paste(cor_H1,"55",sep="")
    cor_poder_transparencia = "#F7F056aa"
    
    titulo = paste(nomelin," x ",nomecol," (n=",total_abcd,", ",i," iteracoes)", sep="")
    # H0
    plot (dens_h0$x[dens_h0$x>=0],dens_h0$y[dens_h0$x>=0], 
          main=titulo, 
          xlab="Valor da Estatistica Qui-Quadrado", 
          ylab="Densidade", 
          col=cor_H0, lwd=3, type="l",
          xlim=c(truncar_x,maxx), ylim=c(0,maxy))
    # H1
    lines (dens_h1, col=cor_H1, lwd=2, lty=2)
    # beta
    d_beta_x <- c(dens_h1$x[dens_h1$x<=q95])
      d_beta_x <- c(min(d_beta_x),d_beta_x,max(d_beta_x))
    d_beta_y <- c(dens_h1$y[dens_h1$x<=q95])
      d_beta_y <- c(0,d_beta_y,0)
    polygon(d_beta_x,d_beta_y,col=cor_beta_transparencia, border=cor_beta_transparencia)
    # poder 
    d_poder_x <- c(dens_h1$x[dens_h1$x>=q95])
      d_poder_x <- c(min(d_poder_x),d_poder_x,max(d_poder_x))
    d_poder_y <- c(dens_h1$y[dens_h1$x>=q95])
      d_poder_y <- c(0,d_poder_y,0)
    polygon(d_poder_x,d_poder_y,col=cor_poder_transparencia, border=cor_poder_transparencia)
    # alfa
    q95 <- quantile(h0_qui,probs=1-alfa, na.rm=TRUE)
    d_alfa_x <- c(dens_h0$x[dens_h0$x>=q95]); 
    d_alfa_x <- c(min(d_alfa_x),d_alfa_x,max(d_alfa_x))
    d_alfa_y <- c(dens_h0$y[dens_h0$x>=q95])
    d_alfa_y <- c(0,d_alfa_y,0)
    polygon(d_alfa_x,d_alfa_y,col=cor_alfa_transparencia, border=cor_alfa_transparencia)
    # cutoff
    lines(c(q95,q95),c(0,maxy), lty=3)
    txtH0 <- paste("H0")
    txtH1 <- paste("H1")
    legend ("topright",c(txtH0,txtH1,"alfa","beta","poder"), 
            lwd=c(3,2,15,15,15), 
            lty=c(1,2,1,1,1), 
            col=c(cor_H0,cor_H1,cor_alfa_transparencia,cor_beta_transparencia,cor_poder_transparencia), 
            box.lwd=0,
            bg="transparent")
    
    
    # pausa para ver o grafico
    if (i>=10 & i <= 100)
    {
      Sys.sleep(0.2)
    }
  }  
  
} # iteracoes

# distribuicao de OR
h1_OR <- sort(h1_OR)
h1_ICOR <- quantile(h1_OR,probs=c(alfa/2,1-(alfa/2)))
d_OR <- density(h1_OR)
medianaOR <- median(h1_OR)

cor_OR = "#ac4d12"
cor_mediana = "#000000"
cor_IC = "#0d5092"

maxx = max(d_OR$x,1)
maxy <- max(d_OR$y)
plot (d_OR, main="Distribuicao do Odds-ratio",
      xlab = "OR", ylab = "Densidade",
      xlim = c(0,maxx), ylim = c(0,maxy),
      col=cor_OR, lwd=3, lty=1
     )
legend ("topright",
        c("OR simul.",
          "mediana",
          paste("IC",round((1-alfa)*100,1),"%",sep=""),
          "referencia"), 
        pch=c(NA,NA,NA,19), 
        lwd=c(3,1,2,NA), 
        lty=c(1,2,1,NA), 
        col=c(cor_OR,cor_mediana,cor_IC,"#000000"), 
        box.lwd=0,
        bg="transparent")
# faixas de OR
# protecao
# 0.1) = grande
# [0.1 a 0.3) = intermediaria
# [0.3 a 0.7) = pequena
# [0.7 a 1) = muito pequena
# neutra
# [1]  = 
# risco
# (1 a 1.5] = muito pequeno 
# (1.5 a 3.5] = pequeno
# (3.5 a 9] = intermediario
# (9 = grande
maxx <- max(d_OR$x)
maxy <- max(d_OR$y)
if (maxx<9) {maxx = 9}
faixa_lim = c(
  0, 
  0.1, 
  0.3, 
  0.7, 
  1, 
  1.5, 
  3.5, 
  9, 
  maxx
)
faixa_txt = c(
  "ef-.gde", # verde musgo
  "ef-.int", # verde folha
  "ef-.peq", # verde claro 1
  "ef-.min", # verde claro 3
  "ef+.min", # tijolo
  "ef+.peq", # terra
  "ef+.int", # marrom claro
  "ef+.gde" # marrom médio
)
# green
# [12] = "#507052"; # verde musgo
# [14] = "#4EB265"; # verde folha
# [15] = "#90C987"; # verde claro 1
# [17] = "#CAE0AB"; # verde claro 3
# red
# [29] = "#f43328"; # tijolo
# [28] = "#a3261b"; # terra
# [26] = "#A5170E"; # marrom claro
# [25] = "#72190E"; # marrom médio
faixa_cor = c(
  "#50705288", # verde musgo
  "#4EB26588", # verde folha
  "#90C98788", # verde claro 1
  "#CAE0AB88", # verde claro 3
  "#f4332888", # tijolo
  "#a3261b88", # terra
  "#A5170E88", # marrom claro
  "#72190E88" # marrom médio
)
for (f in 1:(length(faixa_lim)-1))
{
  polygon(c(faixa_lim[f],faixa_lim[f],faixa_lim[f+1],faixa_lim[f+1]),c(0,maxy/10,maxy/10,0),col=faixa_cor[f], border=faixa_cor[f])
}
# legenda das faixas
leg_txt <- c()
leg_col <- c()
for (f in 1:length(faixa_txt))
{
  if (faixa_lim[f] <= maxx)
  {
    leg_txt <- c(leg_txt,faixa_txt[f])
    leg_col <- c(leg_col,faixa_cor[f])
  }
}
maxx <- max(d_OR$x)
if (medianaOR <= 3.5*maxx/10 | medianaOR >= 6.5*maxx/10) 
{
  # fora do centro
  posleg <- "top"
} else # mediana no centro
{
  if (medianaOR > 5*maxx/10) {posleg <- "topleft"}
  if (medianaOR < 5*maxx/10) {posleg <- "right"}
}
legend (posleg,
        leg_txt, 
        lwd=10, 
        lty=1, 
        col=leg_col, 
        box.lwd=0,
        bg="transparent")
# IC, mediana e referencia
lines(c(h1_ICOR[1],h1_ICOR[1]),c(0,maxy/10),lty=1,lwd=2,col=cor_IC)
lines(c(h1_ICOR[2],h1_ICOR[2]),c(0,maxy/10),lty=1,lwd=2,col=cor_IC)
lines(c(h1_ICOR[1],h1_ICOR[2]),c(maxy/10/2,maxy/10/2),lty=1,lwd=2,col=cor_IC)
lines(c(medianaOR,medianaOR),c(0,maxy),lty=2,lwd=1,col=cor_mediana)
points(c(1),c(maxy/10/2), pch=19,cex=1,col="#000000")

# distribuicao de log(OR)
h1_OR <- sort(h1_OR)
h1_ICOR <- quantile(h1_OR,probs=c(alfa/2,1-(alfa/2)))
d_OR <- density(h1_OR)
medianaOR <- median(h1_OR)

cor_OR = "#ac4d12"
cor_mediana = "#000000"
cor_IC = "#0d5092"

minx = min(d_OR$x[d_OR$x!=0],1e-4)
maxx = max(d_OR$x,1)
maxy <- max(d_OR$y)
plot (d_OR, main="Distribuicao do ln(Odds-ratio)",
      xlab = "ln(OR)", ylab = "Densidade",
      xlim=c(minx,maxx), ylim = c(0,maxy), log = "x",
      col=cor_OR, lwd=3, lty=1
)
legend ("topleft",
        c("ln(OR) simul.",
          "mediana",
          paste("IC",round((1-alfa)*100,1),"%",sep="")
          ), 
        pch=c(NA,NA,NA), 
        lwd=c(3,1,2), 
        lty=c(1,2,1), 
        col=c(cor_OR,cor_mediana,cor_IC), 
        box.lwd=0,
        bg="transparent")
# faixas de OR
# protecao
# 0.1) = grande
# [0.1 a 0.3) = intermediaria
# [0.3 a 0.7) = pequena
# [0.7 a 1) = muito pequena
# neutra
# [1]  = 
# risco
# (1 a 1.5] = muito pequeno 
# (1.5 a 3.5] = pequeno
# (3.5 a 9] = intermediario
# (9 = grande
maxx <- max(d_OR$x)
minx <- max(d_OR$x)
if (minx >= 0.1) {minx = 0.001}
maxy <- max(d_OR$y)
if (maxx<9) {maxx = 9}
faixa_lim = c(
  minx, 
  0.1, 
  0.3, 
  0.7, 
  1, 
  1.5, 
  3.5, 
  9, 
  maxx
)
faixa_txt = c(
  "ef-.gde", # verde musgo
  "ef-.int", # verde folha
  "ef-.peq", # verde claro 1
  "ef-.min", # verde claro 3
  "ef+.min", # tijolo
  "ef+.peq", # terra
  "ef+.int", # marrom claro
  "ef+.gde" # marrom médio
)
# green
# [12] = "#507052"; # verde musgo
# [14] = "#4EB265"; # verde folha
# [15] = "#90C987"; # verde claro 1
# [17] = "#CAE0AB"; # verde claro 3
# red
# [29] = "#f43328"; # tijolo
# [28] = "#a3261b"; # terra
# [26] = "#A5170E"; # marrom claro
# [25] = "#72190E"; # marrom médio
faixa_cor = c(
  "#50705288", # verde musgo
  "#4EB26588", # verde folha
  "#90C98788", # verde claro 1
  "#CAE0AB88", # verde claro 3
  "#f4332888", # tijolo
  "#a3261b88", # terra
  "#A5170E88", # marrom claro
  "#72190E88" # marrom médio
)
for (f in 1:(length(faixa_lim)-1))
{
  polygon(c(faixa_lim[f],faixa_lim[f],faixa_lim[f+1],faixa_lim[f+1]),c(0,maxy/10,maxy/10,0),col=faixa_cor[f], border=faixa_cor[f])
}
# legenda das faixas
leg_txt <- c()
leg_col <- c()
for (f in 1:length(faixa_txt))
{
  if (faixa_lim[f] <= maxx)
  {
    leg_txt <- c(leg_txt,faixa_txt[f])
    leg_col <- c(leg_col,faixa_cor[f])
  }
}
legend ("left",
        leg_txt, 
        lwd=10, 
        lty=1, 
        col=leg_col, 
        box.lwd=0,
        bg="transparent")
# IC, mediana e referencia
lines(c(h1_ICOR[1],h1_ICOR[1]),c(0,maxy/10),lty=1,lwd=2,col=cor_IC)
lines(c(h1_ICOR[2],h1_ICOR[2]),c(0,maxy/10),lty=1,lwd=2,col=cor_IC)
lines(c(h1_ICOR[1],h1_ICOR[2]),c(maxy/10/2,maxy/10/2),lty=1,lwd=2,col=cor_IC)
lines(c(medianaOR,medianaOR),c(0,maxy),lty=2,lwd=1,col=cor_mediana)
# points(c(0),c(maxy/10/2), pch=19,cex=1,col="#000000")
points(minx,maxy/10/2,pch=19,col="black",bg="black")

# distribuicao de V de Cramer
h1_VCramer <- sort(h1_VCramer)
h1_ICV <- quantile(h1_VCramer,probs=c(alfa/2,1-(alfa/2)))
d_V <- density(h1_VCramer)
medianaV <- median(h1_VCramer)

cor_V = "#994F88"
cor_mediana = "#000000"
cor_IC = "#0d5092"

plot (d_V, main="Distribuicao do V de Cramer",
      xlab = "V de Cramer", ylab = "Densidade",
      col=cor_V, lwd=3, lty=1
)
maxy <- max(d_V$y)
legend ("topright",
        c("V simul.",
          "mediana",
          paste("IC",round((1-alfa)*100,1),"%",sep="")
          ), 
        pch=c(NA,NA,NA), 
        lwd=c(3,1,2), 
        lty=c(1,2,1), 
        col=c(cor_V,cor_mediana,cor_IC), 
        box.lwd=0,
        bg="transparent")
# faixas de Cramer
# protecao
# 0.1) = minima
# [0.1 a 0.3) = pequena
# [0.3 a 0.5) = intermediaria
# [0.5 = grande
maxx <- max(d_V$x)
maxy <- max(d_V$y)
if (maxx<0.5) {maxx = 0.6}
faixa_lim = c(
  0, # verde musgo
  0.1, # verde folha
  0.3, # verde claro 1
  0.5, # verde claro 3
  maxx
)
faixa_txt = c(
  "corr.min", # verde musgo
  "corr.peq", # verde folha
  "corr.int", # verde claro 1
  "corr.gde"  # verde claro 3
)
# green
# [12] = "#507052"; # verde musgo
# [14] = "#4EB265"; # verde folha
# [15] = "#90C987"; # verde claro 1
# [17] = "#CAE0AB"; # verde claro 3
faixa_cor = c(
  "#CAE0AB88", # verde claro 3
  "#90C98788", # verde claro 1
  "#4EB26588", # verde folha
  "#50705288" # verde musgo
)
for (f in 1:(length(faixa_lim)-1))
{
  polygon(c(faixa_lim[f],faixa_lim[f],faixa_lim[f+1],faixa_lim[f+1]),c(0,maxy/10,maxy/10,0),col=faixa_cor[f], border=faixa_cor[f])
}
# legenda das faixas
maxx <- max(d_V$x)
leg_txt <- c()
leg_col <- c()
for (f in 1:length(faixa_txt))
{
  if (faixa_lim[f] <= maxx)
  {
    leg_txt <- c(leg_txt,faixa_txt[f])
    leg_col <- c(leg_col,faixa_cor[f])
  }
}

maxx <- max(d_V$x)
if (medianaV <= 3.5*maxx/10 | medianaV >= 6.5*maxx/10) 
{
  # fora do centro
  posleg <- "top"
} else # mediana no centro
{
  if (medianaV > 5*maxx/10) {posleg <- "topleft"}
  if (medianaV < 5*maxx/10) {posleg <- "right"}
}
legend (posleg,
        leg_txt, 
        lwd=10, 
        lty=1, 
        col=leg_col, 
        box.lwd=0,
        bg="transparent")
# IC e mediana
lines(c(h1_ICV[1],h1_ICV[1]),c(0,maxy/10),lty=1,lwd=2,col=cor_IC)
lines(c(h1_ICV[2],h1_ICV[2]),c(0,maxy/10),lty=1,lwd=2,col=cor_IC)
lines(c(h1_ICV[1],h1_ICV[2]),c(maxy/10/2,maxy/10/2),lty=1,lwd=2,col=cor_IC)
maxy <- max(d_V$y)
lines(c(medianaV,medianaV),c(0,maxy),lty=2,lwd=1,col=cor_mediana)

# distribuicao de p
h0_p <- sort(h0_p)
h0_ICp <- quantile(h0_p,probs=c(alfa/2,1-(alfa/2)))
d_p_h0 <- density(h0_p)
medianap_h0 <- median(h0_p)

d_p_h1 <- density(h1_p)
pexato_h1 <- median(h1_p, na.rm = TRUE)

cor_h1 = "#EE8026"
cor_h0 = "#507052"
cor_mediana = "#000000"
cor_IC = "#0d5092"

maxx = max(d_p_h0$x,d_p_h1$x)
maxy <- max(d_p_h0$y,d_p_h1$y)
plot (d_p_h1, main="Distribuicao do valor-p",
      xlab = "p", ylab = "Densidade",
      xlim = c(0,maxx), ylim = c(0,maxy),
      col=cor_h1, lwd=3, lty=1
)
lines (d_p_h0, col=cor_h0, lwd=3, lty=2)
# IC e mediana
lines(c(pexato_h1,pexato_h1),c(0,maxy),lty=2,lwd=1,col=cor_mediana)
legend ("topright",
        c("H0","H1",
          "valor-p exato"
        ),
        pch=c(NA,NA,NA),
        lwd=c(3,3,1),
        lty=c(2,1,2),
        col=c(cor_h0,cor_h1,cor_mediana),
        box.lwd=0,
        bg="transparent")

# Grafico isolando H0, alfa e p
# densidades p sob H0
h0_qui <- sort(h0_qui)
dens_h0 <- density(h0_qui, na.rm=TRUE)

# xlim, ylim
df <- 1

# media da dist. qui^2
mediah0 <- df
dph0 <- sqrt(2*df)
# sugestao para os eixos, evitando cauda muito longa
truncar_x <- 0.05
q95 <- quantile(h0_qui,probs=(1-pexato_h1), na.rm=TRUE)
maxx <- mediah0+3*dph0
if (maxx < as.vector(q95)) {maxx = as.vector(q95)}
maxx <- maxx+1
maxy <- max(dens_h0$y, dens_h1$y)

# quantil 95% (empirico)
q95 <- quantile(h0_qui,probs=1-alfa, na.rm=TRUE)

cor_H0 = "#1965B0"
cor_alfa_transparencia = paste(cor_H0,"88",sep="")
cor_p = "#a30b1b"

titulo = paste(nomelin," x ",nomecol," (n=",total_abcd,", ",i," iteracoes)", sep="")
# H0
plot (dens_h0$x[dens_h0$x>=0],dens_h0$y[dens_h0$x>=0], 
      main=titulo, 
      xlab="Valor da Estatistica Qui-Quadrado", 
      ylab="Densidade", 
      col=cor_H0, lwd=3, type="l",
      xlim=c(truncar_x,maxx), ylim=c(0,maxy))
# alfa
q95 <- quantile(h0_qui,probs=1-alfa, na.rm=TRUE)
d_alfa_x <- c(dens_h0$x[dens_h0$x>=q95]); 
d_alfa_x <- c(min(d_alfa_x),d_alfa_x,max(d_alfa_x))
d_alfa_y <- c(dens_h0$y[dens_h0$x>=q95])
d_alfa_y <- c(0,d_alfa_y,0)
polygon(d_alfa_x,d_alfa_y,col=cor_alfa_transparencia, border=cor_alfa_transparencia)
# p
q_h1 <- quantile(h0_qui,probs=(1-pexato_h1), na.rm=TRUE)
d_p_x <- c(dens_h0$x[dens_h0$x>=q_h1]); 
d_p_x <- c(min(d_p_x),d_p_x,max(d_p_x))
d_p_y <- c(dens_h0$y[dens_h0$x>=q_h1])
d_p_y <- c(0,d_p_y,0)
polygon(d_p_x,d_p_y,col=cor_p, border=cor_p)
lines(c(q_h1,q_h1),c(0,maxy/10), lwd=3, col=cor_p, lty=4)
# cutoff alfa
lines(c(q95,q95),c(0,maxy), lty=3)
txtH0 <- paste("H0")
legend ("topright",c(txtH0,"alfa","p"), 
        lwd=c(3,15,15), 
        lty=c(1,1,1), 
        col=c(cor_H0,cor_alfa_transparencia,cor_p), 
        box.lwd=0,
        bg="transparent")


# calculos
q95 <- quantile(h0_qui,probs=1-alfa, na.rm=TRUE)

cat("\n\nTerminado:\n")
cat("alfa: ",round(alfa*100,3),"%\n", sep="")

cat("\nvalor-p exato:", pexato_h1,"\n")

cat("\nOR:\n")
cat("exato:", medianaOR,"\n")
cat("IC",round((1-alfa)*100,1),"%: [",round(h1_ICOR[1],3),", ",round(h1_ICOR[2],3),"]\n", sep="")

cat("\nV de Cramer:\n")
cat("exato:", medianaV,"\n")
cat("IC",round((1-alfa)*100,1),"%: [",round(h1_ICV[1],3),", ",round(h1_ICV[2],3),"]\n", sep="")

# enable warnings
options(warn=0)

