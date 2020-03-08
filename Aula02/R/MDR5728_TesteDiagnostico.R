# rodando este script np RStudio, observe em Plots

# suppress warnings
options(warn=-1)
# remove all variables
rm(list=ls())

filename <- "CAProstata_USG"
# use filename <- NA para ecoar na tela
# filename <- NA

# cat ("\n\nVoce pode ecoar a saida no console enquanto a simulacao ocorre") 
# cat ("\nou guardar os resultados em arquivos. Neste ultimo caso, ")
# cat ("\nforneca um nome do arquivo valido. ")
# pergunta <- "nome do arquivo (sem extensao): "
# filename <- readline(prompt=pergunta)
# if ( length(filename)<=1 ) 
# {
#   filename <- NA
#   cat ("\n*****************************")
#   cat ("\nRespostas ecoarao na console.")
#   cat ("\n*****************************\n")
# } else
# {
#   cat(paste("filename:",filename,"\n"))
# }

cat ("**** TABELA DE CONTINGENCIA ****\n") 

cat ("\nConsidere:\n") 
output <- data.frame( 
  c1 = c("", "Teste+", "Teste-", "" ),
  c2 = c("Desfecho+", "a", "c", "a+c"),
  c3 = c("Desfecho-", "b", "d", "b+d"),
  c4 = c("", "a+b", "c+d", "a+b+c+d=n")
)
names(output) <- NULL
print(output, row.names = FALSE)

cat ("\n\nInforme:") 
cat ("\n")
cat ("\nP(D+)=(a+c)/n ... prevalencia da Desfecho\n") 
cat ("(numero [0,1]; default = 0.01).\n") 
pergunta <- "prevalencia: "
prevalencia <- readline(prompt=pergunta)
prevalencia <- as.numeric(prevalencia)
if (is.na(prevalencia)) {prevalencia <- 0.01;}
cat(paste("P(D+) =",prevalencia,"\n"))

cat ("\n") 
cat ("Precisamos estabelecer se o Teste se associa com o \n") 
cat ("diagnostico da Desfecho, definindo probabilidades condicionadas\n") 
cat ("\n")
cat ("P(T+|D+) = a/(a+c) ... sensibilidade do teste\n") 
pergunta <- "sensibilidade: "
cat ("(numero [0,1]; default = 0.92).\n") 
sensibilidade <- readline(prompt=pergunta)
sensibilidade <- as.numeric(sensibilidade)
if (is.na(sensibilidade)) {sensibilidade <- 0.92; }
cat(paste("P(T+|D+) =",sensibilidade,"\n"))

cat ("\n")
cat ("P(T-|D-) = d/(b+d) ... especificidade do teste\n") 
pergunta <- "especificidade: "
cat ("(numero [0,1]; default = 0.9).\n") 
especificidade <- readline(prompt=pergunta)
especificidade <- as.numeric(especificidade)
if (is.na(especificidade)) {especificidade <- 0.9}
cat(paste("P(T-|D-) =",especificidade,"\n"))

cat ("\n") 
cat ("Qual o tamanho da população (Total)?\nOpcoes:\n") 
cat ("\t- entre um numero inteiro\n") 
cat ("\t- use n=1 para calcular um indivíduo\n") 
cat ("\t- deixe em branco para definir n adiante\n") 
total <- readline(prompt="n: ")
total <- as.integer(total)
if (is.na(total)) {total <- 0}
alfa <- 0
if (total == 0)
{
  cat ("\n")  
  cat ("Definindo n.\n") 
  cat ("(criterio de parada, default = 1e-4).\n") 
  criterio <- readline(prompt="delta < ")
  criterio <- as.numeric(criterio)
  if (is.na(criterio)) {criterio <- 1e-4}
  cat(paste("criterio de parada com delta <",criterio,"\n"))

  cat ("\n")  
  cat ("Defina alfa = probabilidade do erro do tipo I\n") 
  cat ("(numero entre 0 e 1, default = 0.05).\n") 
  alfa <- readline(prompt="alfa: ")
  alfa <- as.numeric(alfa)
  if (is.na(alfa)) {alfa <- 0.05}
  cat(paste("alfa =",alfa,"\n"))

  cat("\nUm momento, encontrando 'n'\n")

  infp <- c()
  supp <- c()
  infn <- c()
  supn <- c()
  for (total in 1:30000)
  {
    # Construindo tabela
    tabela2x2 <- as.table(matrix(total*c(prevalencia*sensibilidade,       (1 - prevalencia)*(1 - especificidade),
                                         prevalencia*(1 - sensibilidade), (1 - prevalencia)*especificidade), 
                                 nrow = 2, byrow = TRUE))
    colnames(tabela2x2) <- c("D+","D-")
    rownames(tabela2x2) <- c("T+","T-")
    library("epiR")
    out <- epi.tests(tabela2x2, conf.level = 1-alfa)
    sumario <- summary(out)
    
    # print (sumario)
    infp <- c(infp,sumario$lower[11])
    supp <- c(supp,sumario$upper[11])
    infn <- c(infn,sumario$lower[12])
    supn <- c(supn,sumario$upper[12])
    
    if ( (total%%1000) == 0)
    {
      cat(".")
    }
    
    parada <- 0  
    if (total > 1)
    {
      parada <- 1
      if ( 
        abs(infp[total]-infp[total-1]) > criterio || 
        abs(supp[total]-supp[total-1]) > criterio || 
        abs(infn[total]-infn[total-1]) > criterio || 
        abs(supn[total]-supn[total-1]) > criterio 
      )
      {
        parada <- 0
      }
    }
    if (parada == 1)
    {
      break
    }
  }
  x <- 1:length(infp)
  min_y = min(infn[length(infp)],supn[length(infp)],infp[length(infp)],supp[length(infp)])
  max_y = max(infn[length(infp)],supn[length(infp)],infp[length(infp)],supp[length(infp)])
  range_y = max_y-min_y
  plot (c(x,NA,x),c(supp,NA,infp),
        xlab="n",
        ylab="Limites de Likelihood ratio",
        ylim=c(min_y-range_y, max_y+range_y), col="red", type="l")
  lines (c(x,NA,x),c(supn,NA,infn), col="blue",lty=2)
  legend ("bottomright",	
          c("LR+",	"LR-"),	
          pch=c(NA,NA),	
          lwd=c(1,1),	
          lty=c(1,2),	
          col=c("red","blue"),	
          box.lwd=0,	
          bg="transparent")	
}
cat(paste("\nTotal (n) =",total,"\n"))

if (total == 1)
{
  num_iteracoes <- 0
  alfa <- 0.05
  exibetabela <- 0; exibetabelatxt <- "nao"  
} else
{
  if (alfa == 0)
  {
    cat ("\n")  
    cat ("Defina alfa = probabilidade do erro do tipo I\n") 
    cat ("(numero entre 0 e 1, default = 0.05).\n") 
    alfa <- readline(prompt="alfa: ")
    alfa <- as.numeric(alfa)
    if (is.na(alfa)) {alfa <- 0.05}
    cat(paste("alfa =",alfa,"\n"))
    cat ("\n")
  }
  cat ("\n") 
  cat ("Exibir tabelas? (exibir lentifica a simulacao)\n") 
  cat ("(use este recurso apenas com poucas iteracoes)\n") 
  exibetabela <- readline(prompt="0=nao, 1=sim; default eh 0: ")
  exibetabela <- as.integer(exibetabela)
  exibetabelatxt <- "sim"
  if (is.na(exibetabela)) {exibetabela <- 0; exibetabelatxt <- "nao"}
  cat(paste("exibe tabelas =",exibetabelatxt,"\n"))

  cat ("\n") 
  cat ("Quantas iteracoes para simular?\n") 
  cat ("(entre um numero inteiro; default i=1e3\n") 
  cat ("\t- i=0 para finalizar\n") 
  cat ("\t- i>0 para simular\n") 
  num_iteracoes <- readline(prompt="iteracoes: ")
  num_iteracoes <- as.integer(num_iteracoes)
  if (is.na(num_iteracoes)) {num_iteracoes <- 1e3}
  cat(paste("iteracoes =",num_iteracoes,"\n"))
  
}

cat ("\n---------------------") 
cat ("\nIniciando a simulacao") 
cat ("\n---------------------\n") 
cat ("\nAguarde...\n")
if (!is.na(filename))
{
  filetxt <- paste(filename,".txt",sep="")
  filepdf <- paste(filename,".pdf",sep="")
  sink(filetxt)
  pdf(filepdf)
}


# Construindo tabela
cat ("\n\n-------------------------------------------------\n")
tabela2x2 <- as.table(matrix(total*c(prevalencia*sensibilidade,       (1 - prevalencia)*(1 - especificidade),
                                     prevalencia*(1 - sensibilidade), (1 - prevalencia)*especificidade), 
                             nrow = 2, byrow = TRUE))
colnames(tabela2x2) <- c("D+","D-")
rownames(tabela2x2) <- c("T+","T-")
library("epiR")
out <- epi.tests(tabela2x2, conf.level = 1-alfa)
print(out)
sumario <- summary(out)
names(sumario)[1] <- "estimative"
names(sumario)[2] <- paste("IC",round((1-alfa)*100,0)," [lower",sep="" )
names(sumario)[3] <- paste("upper]",sep="" )
row.names(sumario)[ 1] <- paste(row.names(sumario)[ 1],": apparent prevalence, P(T+)",sep="")
row.names(sumario)[ 2] <- paste(row.names(sumario)[ 2],": (true) prevalence, P(D+)",sep="")
row.names(sumario)[ 3] <- paste(row.names(sumario)[ 3],": sensitivity",sep="")
row.names(sumario)[ 4] <- paste(row.names(sumario)[ 4],": specificity",sep="")
row.names(sumario)[ 5] <- paste(row.names(sumario)[ 5],": diag. accuracy",sep="")
row.names(sumario)[ 6] <- paste(row.names(sumario)[ 6],": diag. odds ratio",sep="")
row.names(sumario)[ 7] <- paste(row.names(sumario)[ 7],": number needed to diag.",sep="")
row.names(sumario)[ 8] <- paste(row.names(sumario)[ 8],": Youden's index",sep="")
row.names(sumario)[ 9] <- paste(row.names(sumario)[ 9],": pos. pred. value",sep="")
row.names(sumario)[10] <- paste(row.names(sumario)[10],": neg. pred. value",sep="")
row.names(sumario)[11] <- paste(row.names(sumario)[11],": positive LR",sep="")
row.names(sumario)[12] <- paste(row.names(sumario)[12],": negative LR",sep="")
if (total > 1)
{
  print (sumario)
} else
{
  coluna1 <- c()
  coluna2 <- c()
  for (i in 1:length(row.names(sumario)))
  {
    coluna1 <- c(coluna1,row.names(sumario)[i])
    coluna2 <- c(coluna2,sumario$estimative[i])
  }
  output <- data.frame( 
    c1 = coluna1,
    c2 = coluna2
  )
  names(output) <- c("",names(sumario)[1])
  print (output)
  cat ("---------------------------------------------------------\n")  
}
  
# iterando
h0_qui <- c()
h0_OR <- c()
h0_p <- c()
h0_plr <- c()
h1_qui <- c()
h1_OR <- c()
h1_p <- c()

# distribuicoes epi.tests
h1_aprev	<-	c()
h1_tprev	<-	c()
h1_se	<-	c()
h1_sp	<-	c()
h1_diag.acc	<-	c()
h1_diag.or	<-	c()
h1_nnd	<-	c()
h1_youden	<-	c()
h1_ppv	<-	c()
h1_npv	<-	c()
h1_plr	<-	c()
h1_nlr	<-	c()

h1_VCramer <- c()

tabela2x2_tmp <- tabela2x2

if (num_iteracoes > 0)
{
  
  # exibe H0 e H1
  nomecol <- "D"
  nomelin <- "T"
  # H1
  tabela2x2_propH1 <- tabela2x2/total
  out <- epi.tests(tabela2x2_propH1, conf.level = 1-alfa)
  sumarioH1 <- summary(out)
  
  # H0
  tabela2x2_propH0 <- tabela2x2
  tabela2x2_propH0[1] <- (tabela2x2_propH1[1]+tabela2x2_propH1[3])*(tabela2x2_propH1[1]+tabela2x2_propH1[2])
  tabela2x2_propH0[3] <- (tabela2x2_propH1[1]+tabela2x2_propH1[3])*(tabela2x2_propH1[3]+tabela2x2_propH1[4])
  tabela2x2_propH0[2] <- (tabela2x2_propH1[1]+tabela2x2_propH1[2])*(tabela2x2_propH1[2]+tabela2x2_propH1[4])
  tabela2x2_propH0[4] <- (tabela2x2_propH1[2]+tabela2x2_propH1[4])*(tabela2x2_propH1[3]+tabela2x2_propH1[4])
  out <- epi.tests(tabela2x2_propH0, conf.level = 1-alfa)
  sumarioH0 <- summary(out)
  
  cat ("\n") 
  cat ("Sob H0 espera-se (em proporcoes):\n") 
  output <- data.frame(
    c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
    c2 = c(paste(nomecol,"+"), round(tabela2x2_propH0[1],3), round(tabela2x2_propH0[2],3), round((tabela2x2_propH0[1]+tabela2x2_propH0[2]),3)),
    c3 = c(paste(nomecol,"-"), round(tabela2x2_propH0[3],3), round(tabela2x2_propH0[4],3), round((tabela2x2_propH0[3]+tabela2x2_propH0[4]),3)),
    c4 = c("", round(tabela2x2_propH0[1]+tabela2x2_propH0[3],3), round(tabela2x2_propH0[2]+tabela2x2_propH0[4],3), "")
  )
  names(output) <- NULL
  print(output, row.names = FALSE)
  cat ("\n") 
  cat ("Sob H1 espera-se (em proporcoes):\n") 
  output <- data.frame(
    c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
    c2 = c(paste(nomecol,"+"), round(tabela2x2_propH1[1],3), round(tabela2x2_propH1[2],3), round((tabela2x2_propH1[1]+tabela2x2_propH1[2]),3)),
    c3 = c(paste(nomecol,"-"), round(tabela2x2_propH1[3],3), round(tabela2x2_propH1[4],3), round((tabela2x2_propH1[3]+tabela2x2_propH1[4]),3)),
    c4 = c("", round(tabela2x2_propH1[1]+tabela2x2_propH1[3],3), round(tabela2x2_propH1[2]+tabela2x2_propH1[4],3), "")
  )
  names(output) <- NULL
  print(output, row.names = FALSE)
  
  # valores de referencia
  h0_ref <- c(tabela2x2_propH0[1],tabela2x2_propH0[3],tabela2x2_propH0[2],tabela2x2_propH0[4])
  h0_probs <- c()
  acm <- 0
  for (idx in 1:4)
  {
    acm <- acm+h0_ref[idx]
    h0_probs <- c(h0_probs, acm)
  }
  h1_ref <- c(tabela2x2_propH1[1],tabela2x2_propH1[3],tabela2x2_propH1[2],tabela2x2_propH1[4])
  h1_probs <- c()
  acm <- 0
  for (idx in 1:4)
  {
    acm <- acm+h1_ref[idx]
    h1_probs <- c(h1_probs, acm)
  }
  
  cat("\n")
  cat("\namostra =",total)
  cat ("\nalfa =",alfa)
  cat ("\niteracoes =",num_iteracoes)
  
  # com que frequencia exibe o grafico (100 graficos)
  exibegrf <- num_iteracoes/100; 
  
  for (i in 1:num_iteracoes)
  {
    # distribuindo os individuos sob H0  
    aloca_h0 <- c(0,0,0,0)
    for (a in 1:total)
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
      aloca_h0[idx] <- aloca_h0[idx]/total 
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
    qui2_h0 <- phi_h0 * total
    h0_qui <- c(h0_qui,qui2_h0)
    h0_p <- c(h0_p,1-pchisq(qui2_h0,1))
    
    OR_h0 <- (aloca_h0[1]*aloca_h0[4])/(aloca_h0[2]*aloca_h0[3])
    if (is.finite(OR_h0))
    {
      h0_OR <- c(h0_OR,OR_h0)
    }
    tabela2x2_tmp[1] <- h0_sml_obs[1]
    tabela2x2_tmp[2] <- h0_sml_obs[3]
    tabela2x2_tmp[3] <- h0_sml_obs[2]
    tabela2x2_tmp[4] <- h0_sml_obs[4]
    out <- epi.tests(tabela2x2_tmp, conf.level = 1-alfa)
    sumario <- summary(out)
    h0_plr <- c(h0_plr,sumario$est[11])
    if (exibetabela == 1)
    {
      cat("\nH0:\n")
      print (out)
      print(sumario)
    }
    # aprev      0.70380000  3.972124e-03    0.9999980
    # tprev      0.64210000  2.262539e-03    0.9999821
    # se         0.69880081  2.159783e-04    1.0000000
    # sp         0.28723107  2.220446e-16    0.9999995
    # diag.acc   0.55150000  8.070746e-04    0.9998428
    # diag.or    0.93493680  1.163399e-04 7513.3854935
    # nnd      -71.59159559 -1.000216e+00    1.0000005
    # youden    -0.01396812 -9.997840e-01    0.9999995
    # ppv        0.63753907  2.031398e-04    0.9999997
    # npv        0.34706280  2.220446e-16    1.0000000
    # plr        0.98040302  7.083997e-02   13.5684703
    # nlr        1.04863025  1.804161e-03  609.4940401
    ppv <- 
      # distribuindo os individuos sob H1  
      aloca_h1 <- c(0,0,0,0)
    for (a in 1:total)
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
      aloca_h1[idx] <- aloca_h1[idx]/total 
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
    qui2_h1 <- phi_h1 * total
    h1_qui <- c(h1_qui,qui2_h1)
    h1_p <- c(h1_p,1-pchisq(qui2_h1,1))
    
    OR_h1 <- (aloca_h1[1]*aloca_h1[4])/(aloca_h1[2]*aloca_h1[3])
    if (is.finite(OR_h1))
    {
      h1_OR <- c(h1_OR,OR_h1)
    }
    tabela2x2_tmp[1] <- h1_sml_obs[1]
    tabela2x2_tmp[2] <- h1_sml_obs[3]
    tabela2x2_tmp[3] <- h1_sml_obs[2]
    tabela2x2_tmp[4] <- h1_sml_obs[4]
    out <- epi.tests(tabela2x2_tmp, conf.level = 1-alfa)
    sumario <- summary(out)
    h1_aprev	<-	c(h1_aprev, sumario$est[1])
    h1_tprev	<-	c(h1_tprev, sumario$est[2])
    h1_se	<-	c(h1_se, sumario$est[3])
    h1_sp	<-	c(h1_sp, sumario$est[4])
    h1_diag.acc	<-	c(h1_diag.acc, sumario$est[5])
    h1_diag.or	<-	c(h1_diag.or, sumario$est[6])
    h1_nnd	<-	c(h1_nnd, sumario$est[7])
    h1_youden	<-	c(h1_youden, sumario$est[8])
    h1_ppv	<-	c(h1_ppv, sumario$est[9])
    h1_npv	<-	c(h1_npv, sumario$est[10])
    h1_plr	<-	c(h1_plr, sumario$est[11])
    h1_nlr	<-	c(h1_nlr, sumario$est[12])
    if (exibetabela == 1)
    {
      cat("\nH1:\n")
      print (out)
      print(sumario)
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
        c2 = c(paste(nomecol,"+"), round(aloca_h0[1]*total,0), round(aloca_h0[3]*total,0), round((aloca_h0[1]+aloca_h0[3])*total,0)),
        c3 = c(paste(nomecol,"-"), round(aloca_h0[2]*total,0), round(aloca_h0[4]*total,0), round((aloca_h0[2]+aloca_h0[4])*total,0)),
        c4 = c("", round((aloca_h0[1]+aloca_h0[2])*total,0), round((aloca_h0[3]+aloca_h0[4])*total,0), total )
      )
      names(output) <- NULL
      print(output, row.names = FALSE)
      cat ("\n--- esperados:") 
      output <- data.frame(
        c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
        c2 = c(paste(nomecol,"+"), round(h0_sml_esp[1]*total,3), round(h0_sml_esp[3]*total,3), round((h0_sml_esp[1]+h0_sml_esp[3])*total,3)),
        c3 = c(paste(nomecol,"-"), round(h0_sml_esp[2]*total,3), round(h0_sml_esp[4]*total,3), round((h0_sml_esp[2]+h0_sml_esp[4])*total,3)),
        c4 = c("", round((h0_sml_esp[1]+h0_sml_esp[2])*total,3), round((h0_sml_esp[3]+h0_sml_esp[4])*total,3), total)
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
        c2 = c(paste(nomecol,"+"), round(aloca_h1[1]*total,0), round(aloca_h1[3]*total,0), round((aloca_h1[1]+aloca_h1[3])*total,0)),
        c3 = c(paste(nomecol,"-"), round(aloca_h1[2]*total,0), round(aloca_h1[4]*total,0), round((aloca_h1[2]+aloca_h1[4])*total,0)),
        c4 = c("", round((aloca_h1[1]+aloca_h1[2])*total,0), round((aloca_h1[3]+aloca_h1[4])*total,0), total )
      )
      names(output) <- NULL
      print(output, row.names = FALSE)
      cat ("\n--- esperados:") 
      output <- data.frame(
        c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
        c2 = c(paste(nomecol,"+"), round(h1_sml_esp[1]*total,3), round(h1_sml_esp[3]*total,3), round((h1_sml_esp[1]+h1_sml_esp[3])*total,3)),
        c3 = c(paste(nomecol,"-"), round(h1_sml_esp[2]*total,3), round(h1_sml_esp[4]*total,3), round((h1_sml_esp[2]+h1_sml_esp[4])*total,3)),
        c4 = c("", round((h1_sml_esp[1]+h1_sml_esp[2])*total,3), round((h1_sml_esp[3]+h1_sml_esp[4])*total,3), total)
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
    
    if  ( is.na(filename) && length(h0_qui)>=2 & ((i %% exibegrf) == 0 || (i>=10 & i <= 100)) )
    {
      # pausa para ver o grafico
      tempo <- 0.5
      if (i<=100) {tempo <- 0.1}
      
      #	nlr
      # distribuicao de nlr (LR-)	
      h1_nlr <- sort(h1_nlr)	
      h1_ICnlr <- quantile(h1_nlr,probs=c(alfa/2,1-(alfa/2)))	
      d_nlr <- density(h1_nlr)	
      mediananlr <- median(h1_nlr)	
      cor_nlr = "#ac4d12"	
      cor_mediana = "#000000"	
      cor_IC = "#0d5092"	
      cor_OR = "#ac4d12"
      
      maxx = max(d_nlr$x)	
      maxy <- max(d_nlr$y)	
      plot (d_nlr, main=paste("Distribuicao de LR- (iteracoes: ",i,")",sep=""),	
            xlab = "LR-", ylab = "Densidade",	
            # xlim = c(0,maxx), ylim = c(0,maxy),	
            col=cor_OR, lwd=3, lty=1)	
      lines(	
        c(h1_ICnlr[1],h1_ICnlr[1],h1_ICnlr[1],h1_ICnlr[2],h1_ICnlr[2],h1_ICnlr[2]),	
        c(0,maxy/20,maxy/40,maxy/40,maxy/20,0),	
        col=cor_IC, lwd=2)	
      points(1,maxy/40,col="#000000", pch=19)	
      lines(c(mediananlr,mediananlr),c(0,maxy),col=cor_mediana,lty=2,lwd=1)	
      legend ("topleft",	
              c("LR- simul.",	
                paste("mediana = ",round(mediananlr,5),sep=""),	
                paste("IC",round((1-alfa)*100,1),"% [",round(h1_ICnlr[1],2),", ",round(h1_ICnlr[2],2),"]",sep="")	
              ),	
              pch=c(NA,NA,NA,19),	
              lwd=c(3,1,2,NA),	
              lty=c(1,2,1,NA),	
              col=c(cor_nlr,cor_mediana,cor_IC,"#000000"),	
              box.lwd=0,	
              bg="transparent")	
      # pausa para ver o grafico
      Sys.sleep(tempo)
      
      #	plr
      # distribuicao de plr (LR+)	
      h1_plr <- sort(h1_plr)	
      h1_ICplr <- quantile(h1_plr,probs=c(alfa/2,1-(alfa/2)))	
      d_plr <- density(h1_plr)	
      medianaplr <- median(h1_plr)	
      cor_plr = "#ac4d12"	
      cor_mediana = "#000000"	
      cor_IC = "#0d5092"	
      cor_OR = "#ac4d12"
      
      maxx = max(d_plr$x)	
      maxy <- max(d_plr$y)	
      plot (d_plr, main=paste("Distribuicao de LR+ (iteracoes: ",i,")",sep=""),	
            xlab = "LR+", ylab = "Densidade",	
            # xlim = c(0,maxx), ylim = c(0,maxy),	
            col=cor_OR, lwd=3, lty=1)	
      lines(	
        c(h1_ICplr[1],h1_ICplr[1],h1_ICplr[1],h1_ICplr[2],h1_ICplr[2],h1_ICplr[2]),	
        c(0,maxy/20,maxy/40,maxy/40,maxy/20,0),	
        col=cor_IC, lwd=2)	
      points(1,maxy/40,col="#000000", pch=19)	
      lines(c(medianaplr,medianaplr),c(0,maxy),col=cor_mediana,lty=2,lwd=1)	
      legend ("topleft",	
              c("LR+ simul.",	
                paste("mediana = ",round(medianaplr,5),sep=""),	
                paste("IC",round((1-alfa)*100,1),"% [",round(h1_ICplr[1],2),", ",round(h1_ICplr[2],2),"]",sep="")	
              ),	
              pch=c(NA,NA,NA,19),	
              lwd=c(3,1,2,NA),	
              lty=c(1,2,1,NA),	
              col=c(cor_plr,cor_mediana,cor_IC,"#000000"),	
              box.lwd=0,	
              bg="transparent")	
      
      # pausa para ver o grafico
      Sys.sleep(tempo*3)
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
  plot (d_OR, main=paste("Distribuicao do Odds-ratio",", n=",total,sep=""),
        xlab = "OR", ylab = "Densidade",
        xlim = c(0,maxx), ylim = c(0,maxy),
        col=cor_OR, lwd=3, lty=1
  )
  legend ("topleft",
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
  minx <- min(d_OR$x)
  maxx <- max(d_OR$x)
  if (medianaOR <= minx+(2.5*(maxx-minx)/10) | 
      medianaOR >= minx+(7.5*(maxx-minx)/10)) 
  {
    # fora do centro
    posleg <- "top"
  } else # mediana no centro
  {
    if (medianaOR >= minx+(5*(maxx-minx)/10)) {posleg <- "topleft"}
    if (medianaOR < minx+(5*(maxx-minx)/10)) {posleg <- "right"}
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
  
  minx = max(d_OR$x[d_OR$x!=0],1)
  maxx = max(d_OR$x,1)
  maxy <- max(d_OR$y)
  plot (d_OR, main=paste("Distribuicao do ln(Odds-ratio)",", n=",total,sep=""),
        xlab = "ln(OR)", ylab = "Densidade",
        ylim = c(0,maxy), log = "x",
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
  
  # distribuicao de V de Cramer
  h1_VCramer <- sort(h1_VCramer)
  h1_ICV <- quantile(h1_VCramer,probs=c(alfa/2,1-(alfa/2)))
  d_V <- density(h1_VCramer)
  medianaV <- median(h1_VCramer)
  
  cor_V = "#994F88"
  cor_mediana = "#000000"
  cor_IC = "#0d5092"
  
  plot (d_V, main=paste("Distribuicao do V de Cramer",", n=",total,sep=""),
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
  
  minx <- min(d_V$x)
  maxx <- max(d_V$x)
  if (medianaV <= minx+(2.5*(maxx-minx)/10) | 
      medianaV >= minx+(7.5*(maxx-minx)/10)) 
  {
    # fora do centro
    posleg <- "top"
  } else # mediana no centro
  {
    if (medianaV >= minx+(5*(maxx-minx)/10)) {posleg <- "topleft"}
    if (medianaV < minx+(5*(maxx-minx)/10)) {posleg <- "right"}
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
  
  #	aprev
  # distribuicao de aprev (Prev. Ap.)	
  h1_aprev <- sort(h1_aprev)	
  h1_ICaprev <- quantile(h1_aprev,probs=c(alfa/2,1-(alfa/2)))	
  d_aprev <- density(h1_aprev)	
  medianaaprev <- median(h1_aprev)	
  cor_aprev = "#ac4d12"	
  cor_mediana = "#000000"	
  cor_IC = "#0d5092"	
  maxx = max(d_aprev$x)	
  maxy <- max(d_aprev$y)	
  plot (d_aprev, main=paste("Distribuicao de Prev. Ap.",", n=",total,sep=""),	
        xlab = "Prev. Ap.", ylab = "Densidade",	
        # xlim = c(0,maxx), ylim = c(0,maxy),	
        col=cor_OR, lwd=3, lty=1)	
  lines(	
    c(h1_ICaprev[1],h1_ICaprev[1],h1_ICaprev[1],h1_ICaprev[2],h1_ICaprev[2],h1_ICaprev[2]),	
    c(0,maxy/20,maxy/40,maxy/40,maxy/20,0),	
    col=cor_IC, lwd=2)	
  # points(1,maxy/40,col="#000000", pch=19)	
  lines(c(medianaaprev,medianaaprev),c(0,maxy),col=cor_mediana,lty=2,lwd=1)	
  legend ("topleft",	
          c("Prev. Ap. simul.",	
            paste("mediana = ",round(medianaaprev,5),sep=""),	
            paste("IC",round((1-alfa)*100,1),"%",sep="")	
          ),	
          pch=c(NA,NA,NA,19),	
          lwd=c(3,1,2,NA),	
          lty=c(1,2,1,NA),	
          col=c(cor_aprev,cor_mediana,cor_IC,"#000000"),	
          box.lwd=0,	
          bg="transparent")	
  
  #	tprev
  # distribuicao de tprev (Prev. Obs.)	
  h1_tprev <- sort(h1_tprev)	
  h1_ICtprev <- quantile(h1_tprev,probs=c(alfa/2,1-(alfa/2)))	
  d_tprev <- density(h1_tprev)	
  medianatprev <- median(h1_tprev)	
  cor_tprev = "#ac4d12"	
  cor_mediana = "#000000"	
  cor_IC = "#0d5092"	
  maxx = max(d_tprev$x)	
  maxy <- max(d_tprev$y)	
  plot (d_tprev, paste(main="Distribuicao de Prev. Obs.",", n=",total,sep=""),	
        xlab = "Prev. Obs.", ylab = "Densidade",	
        # xlim = c(0,maxx), ylim = c(0,maxy),	
        col=cor_OR, lwd=3, lty=1)	
  lines(	
    c(h1_ICtprev[1],h1_ICtprev[1],h1_ICtprev[1],h1_ICtprev[2],h1_ICtprev[2],h1_ICtprev[2]),	
    c(0,maxy/20,maxy/40,maxy/40,maxy/20,0),	
    col=cor_IC, lwd=2)	
  # points(1,maxy/40,col="#000000", pch=19)	
  lines(c(medianatprev,medianatprev),c(0,maxy),col=cor_mediana,lty=2,lwd=1)	
  legend ("topleft",	
          c("Prev. Obs. simul.",	
            paste("mediana = ",round(medianatprev,5),sep=""),	
            paste("IC",round((1-alfa)*100,1),"%",sep="")	
          ),	
          pch=c(NA,NA,NA,19),	
          lwd=c(3,1,2,NA),	
          lty=c(1,2,1,NA),	
          col=c(cor_tprev,cor_mediana,cor_IC,"#000000"),	
          box.lwd=0,	
          bg="transparent")	
  
  #	se
  # distribuicao de se (Sensibilidade)	
  h1_se <- sort(h1_se)	
  h1_ICse <- quantile(h1_se,probs=c(alfa/2,1-(alfa/2)))	
  d_se <- density(h1_se)	
  medianase <- median(h1_se)	
  cor_se = "#ac4d12"	
  cor_mediana = "#000000"	
  cor_IC = "#0d5092"	
  maxx = max(d_se$x)	
  maxy <- max(d_se$y)	
  plot (d_se, main=paste("Distribuicao de Sensibilidade",", n=",total,sep=""),	
        xlab = "Sensibilidade", ylab = "Densidade",	
        # xlim = c(0,maxx), ylim = c(0,maxy),	
        col=cor_OR, lwd=3, lty=1)	
  lines(	
    c(h1_ICse[1],h1_ICse[1],h1_ICse[1],h1_ICse[2],h1_ICse[2],h1_ICse[2]),	
    c(0,maxy/20,maxy/40,maxy/40,maxy/20,0),	
    col=cor_IC, lwd=2)	
  # points(1,maxy/40,col="#000000", pch=19)	
  lines(c(medianase,medianase),c(0,maxy),col=cor_mediana,lty=2,lwd=1)	
  legend ("topleft",	
          c("Sensibilidade simul.",	
            paste("mediana = ",round(medianase,5),sep=""),	
            paste("IC",round((1-alfa)*100,1),"%",sep="")	
          ),	
          pch=c(NA,NA,NA,19),	
          lwd=c(3,1,2,NA),	
          lty=c(1,2,1,NA),	
          col=c(cor_se,cor_mediana,cor_IC,"#000000"),	
          box.lwd=0,	
          bg="transparent")	
  
  #	sp
  # distribuicao de sp (Especificidade)	
  h1_sp <- sort(h1_sp)	
  h1_ICsp <- quantile(h1_sp,probs=c(alfa/2,1-(alfa/2)))	
  d_sp <- density(h1_sp)	
  medianasp <- median(h1_sp)	
  cor_sp = "#ac4d12"	
  cor_mediana = "#000000"	
  cor_IC = "#0d5092"	
  maxx = max(d_sp$x)	
  maxy <- max(d_sp$y)	
  plot (d_sp, main=paste("Distribuicao de Especificidade",", n=",total,sep=""),	
        xlab = "Especificidade", ylab = "Densidade",	
        # xlim = c(0,maxx), ylim = c(0,maxy),	
        col=cor_OR, lwd=3, lty=1)	
  lines(	
    c(h1_ICsp[1],h1_ICsp[1],h1_ICsp[1],h1_ICsp[2],h1_ICsp[2],h1_ICsp[2]),	
    c(0,maxy/20,maxy/40,maxy/40,maxy/20,0),	
    col=cor_IC, lwd=2)	
  # points(1,maxy/40,col="#000000", pch=19)	
  lines(c(medianasp,medianasp),c(0,maxy),col=cor_mediana,lty=2,lwd=1)	
  legend ("topleft",	
          c("Especificidade simul.",	
            paste("mediana = ",round(medianasp,5),sep=""),	
            paste("IC",round((1-alfa)*100,1),"%",sep="")	
          ),	
          pch=c(NA,NA,NA,19),	
          lwd=c(3,1,2,NA),	
          lty=c(1,2,1,NA),	
          col=c(cor_sp,cor_mediana,cor_IC,"#000000"),	
          box.lwd=0,	
          bg="transparent")	
  
  #	diag.acc
  # distribuicao de diag.acc (Acuracia)	
  h1_diag.acc <- sort(h1_diag.acc)	
  h1_ICdiag.acc <- quantile(h1_diag.acc,probs=c(alfa/2,1-(alfa/2)))	
  d_diag.acc <- density(h1_diag.acc)	
  medianadiag.acc <- median(h1_diag.acc)	
  cor_diag.acc = "#ac4d12"	
  cor_mediana = "#000000"	
  cor_IC = "#0d5092"	
  maxx = max(d_diag.acc$x)	
  maxy <- max(d_diag.acc$y)	
  plot (d_diag.acc, paste(main="Distribuicao de Acuracia",", n=",total,sep=""),	
        xlab = "Acuracia", ylab = "Densidade",	
        # xlim = c(0,maxx), ylim = c(0,maxy),	
        col=cor_OR, lwd=3, lty=1)	
  lines(	
    c(h1_ICdiag.acc[1],h1_ICdiag.acc[1],h1_ICdiag.acc[1],h1_ICdiag.acc[2],h1_ICdiag.acc[2],h1_ICdiag.acc[2]),	
    c(0,maxy/20,maxy/40,maxy/40,maxy/20,0),	
    col=cor_IC, lwd=2)	
  # points(1,maxy/40,col="#000000", pch=19)	
  lines(c(medianadiag.acc,medianadiag.acc),c(0,maxy),col=cor_mediana,lty=2,lwd=1)	
  legend ("topleft",	
          c("Acuracia simul.",	
            paste("mediana = ",round(medianadiag.acc,5),sep=""),	
            paste("IC",round((1-alfa)*100,1),"%",sep="")	
          ),	
          pch=c(NA,NA,NA,19),	
          lwd=c(3,1,2,NA),	
          lty=c(1,2,1,NA),	
          col=c(cor_diag.acc,cor_mediana,cor_IC,"#000000"),	
          box.lwd=0,	
          bg="transparent")	
  
  #	diag.or
  # distribuicao de diag.or (Odds ratio)	
  h1_diag.or <- sort(h1_diag.or)	
  h1_ICdiag.or <- quantile(h1_diag.or,probs=c(alfa/2,1-(alfa/2)))	
  d_diag.or <- density(h1_diag.or)	
  medianadiag.or <- median(h1_diag.or)	
  cor_diag.or = "#ac4d12"	
  cor_mediana = "#000000"	
  cor_IC = "#0d5092"	
  maxx = max(d_diag.or$x)	
  maxy <- max(d_diag.or$y)	
  plot (d_diag.or, main=paste("Distribuicao de Odds ratio",", n=",total,sep=""),	
        xlab = "Odds ratio", ylab = "Densidade",	
        # xlim = c(0,maxx), ylim = c(0,maxy),	
        col=cor_OR, lwd=3, lty=1)	
  lines(	
    c(h1_ICdiag.or[1],h1_ICdiag.or[1],h1_ICdiag.or[1],h1_ICdiag.or[2],h1_ICdiag.or[2],h1_ICdiag.or[2]),	
    c(0,maxy/20,maxy/40,maxy/40,maxy/20,0),	
    col=cor_IC, lwd=2)	
  # points(1,maxy/40,col="#000000", pch=19)	
  lines(c(medianadiag.or,medianadiag.or),c(0,maxy),col=cor_mediana,lty=2,lwd=1)	
  legend ("topleft",	
          c("Odds ratio simul.",	
            paste("mediana = ",round(medianadiag.or,5),sep=""),	
            paste("IC",round((1-alfa)*100,1),"%",sep="")	
          ),	
          pch=c(NA,NA,NA,19),	
          lwd=c(3,1,2,NA),	
          lty=c(1,2,1,NA),	
          col=c(cor_diag.or,cor_mediana,cor_IC,"#000000"),	
          box.lwd=0,	
          bg="transparent")	
  
  #	nnd
  # distribuicao de nnd (nnd)	
  h1_nnd <- sort(h1_nnd)	
  h1_ICnnd <- quantile(h1_nnd,probs=c(alfa/2,1-(alfa/2)))	
  d_nnd <- density(h1_nnd)	
  medianannd <- median(h1_nnd)	
  cor_nnd = "#ac4d12"	
  cor_mediana = "#000000"	
  cor_IC = "#0d5092"	
  maxx = max(d_nnd$x)	
  maxy <- max(d_nnd$y)	
  plot (d_nnd, main=paste("Distribuicao de nnd",", n=",total,sep=""),	
        xlab = "nnd", ylab = "Densidade",	
        # xlim = c(0,maxx), ylim = c(0,maxy),	
        col=cor_OR, lwd=3, lty=1)	
  lines(	
    c(h1_ICnnd[1],h1_ICnnd[1],h1_ICnnd[1],h1_ICnnd[2],h1_ICnnd[2],h1_ICnnd[2]),	
    c(0,maxy/20,maxy/40,maxy/40,maxy/20,0),	
    col=cor_IC, lwd=2)	
  # points(1,maxy/40,col="#000000", pch=19)	
  lines(c(medianannd,medianannd),c(0,maxy),col=cor_mediana,lty=2,lwd=1)	
  legend ("topleft",	
          c("nnd simul.",	
            paste("mediana = ",round(medianannd,5),sep=""),	
            paste("IC",round((1-alfa)*100,1),"%",sep="")	
          ),	
          pch=c(NA,NA,NA,19),	
          lwd=c(3,1,2,NA),	
          lty=c(1,2,1,NA),	
          col=c(cor_nnd,cor_mediana,cor_IC,"#000000"),	
          box.lwd=0,	
          bg="transparent")	
  
  #	youden
  # distribuicao de youden (Youden's index)	
  h1_youden <- sort(h1_youden)	
  h1_ICyouden <- quantile(h1_youden,probs=c(alfa/2,1-(alfa/2)))	
  d_youden <- density(h1_youden)	
  medianayouden <- median(h1_youden)	
  cor_youden = "#ac4d12"	
  cor_mediana = "#000000"	
  cor_IC = "#0d5092"	
  maxx = max(d_youden$x)	
  maxy <- max(d_youden$y)	
  plot (d_youden, main=paste("Distribuicao de Youden's index.",", n=",total,sep=""),	
        xlab = "Youden's index.", ylab = "Densidade",	
        # xlim = c(0,maxx), ylim = c(0,maxy),	
        col=cor_OR, lwd=3, lty=1)	
  lines(	
    c(h1_ICyouden[1],h1_ICyouden[1],h1_ICyouden[1],h1_ICyouden[2],h1_ICyouden[2],h1_ICyouden[2]),	
    c(0,maxy/20,maxy/40,maxy/40,maxy/20,0),	
    col=cor_IC, lwd=2)	
  # points(1,maxy/40,col="#000000", pch=19)	
  lines(c(medianayouden,medianayouden),c(0,maxy),col=cor_mediana,lty=2,lwd=1)	
  legend ("topleft",	
          c("Youden's index. simul.",	
            paste("mediana = ",round(medianayouden,5),sep=""),	
            paste("IC",round((1-alfa)*100,1),"%",sep="")	
          ),	
          pch=c(NA,NA,NA,19),	
          lwd=c(3,1,2,NA),	
          lty=c(1,2,1,NA),	
          col=c(cor_youden,cor_mediana,cor_IC,"#000000"),	
          box.lwd=0,	
          bg="transparent")	
  
  #	ppv
  # distribuicao de ppv (VPP)	
  h1_ppv <- sort(h1_ppv)	
  h1_ICppv <- quantile(h1_ppv,probs=c(alfa/2,1-(alfa/2)))	
  d_ppv <- density(h1_ppv)	
  medianappv <- median(h1_ppv)	
  cor_ppv = "#ac4d12"	
  cor_mediana = "#000000"	
  cor_IC = "#0d5092"	
  maxx = max(d_ppv$x)	
  maxy <- max(d_ppv$y)	
  plot (d_ppv, main=paste("Distribuicao de VPP",", n=",total,sep=""),	
        xlab = "VPP", ylab = "Densidade",	
        # xlim = c(0,maxx), ylim = c(0,maxy),	
        col=cor_OR, lwd=3, lty=1)	
  lines(	
    c(h1_ICppv[1],h1_ICppv[1],h1_ICppv[1],h1_ICppv[2],h1_ICppv[2],h1_ICppv[2]),	
    c(0,maxy/20,maxy/40,maxy/40,maxy/20,0),	
    col=cor_IC, lwd=2)	
  # points(1,maxy/40,col="#000000", pch=19)	
  lines(c(medianappv,medianappv),c(0,maxy),col=cor_mediana,lty=2,lwd=1)	
  legend ("topleft",	
          c("VPP simul.",	
            paste("mediana = ",round(medianappv,5),sep=""),	
            paste("IC",round((1-alfa)*100,1),"%",sep="")	
          ),	
          pch=c(NA,NA,NA,19),	
          lwd=c(3,1,2,NA),	
          lty=c(1,2,1,NA),	
          col=c(cor_ppv,cor_mediana,cor_IC,"#000000"),	
          box.lwd=0,	
          bg="transparent")	
  
  #	npv
  # distribuicao de npv (VPN)	
  h1_npv <- sort(h1_npv)	
  h1_ICnpv <- quantile(h1_npv,probs=c(alfa/2,1-(alfa/2)))	
  d_npv <- density(h1_npv)	
  mediananpv <- median(h1_npv)	
  cor_npv = "#ac4d12"	
  cor_mediana = "#000000"	
  cor_IC = "#0d5092"	
  maxx = max(d_npv$x)	
  maxy <- max(d_npv$y)	
  plot (d_npv, main=paste("Distribuicao de VPN",", n=",total,sep=""),	
        xlab = "VPN", ylab = "Densidade",	
        # xlim = c(0,maxx), ylim = c(0,maxy),	
        col=cor_OR, lwd=3, lty=1)	
  lines(	
    c(h1_ICnpv[1],h1_ICnpv[1],h1_ICnpv[1],h1_ICnpv[2],h1_ICnpv[2],h1_ICnpv[2]),	
    c(0,maxy/20,maxy/40,maxy/40,maxy/20,0),	
    col=cor_IC, lwd=2)	
  # points(1,maxy/40,col="#000000", pch=19)	
  lines(c(mediananpv,mediananpv),c(0,maxy),col=cor_mediana,lty=2,lwd=1)	
  legend ("topleft",	
          c("VPN simul.",	
            paste("mediana = ",round(mediananpv,5),sep=""),	
            paste("IC",round((1-alfa)*100,1),"%",sep="")	
          ),	
          pch=c(NA,NA,NA,19),	
          lwd=c(3,1,2,NA),	
          lty=c(1,2,1,NA),	
          col=c(cor_npv,cor_mediana,cor_IC,"#000000"),	
          box.lwd=0,	
          bg="transparent")	
  
  #	plr
  # distribuicao de plr (LR+)	
  h1_plr <- sort(h1_plr)	
  h1_ICplr <- quantile(h1_plr,probs=c(alfa/2,1-(alfa/2)))	
  d_plr <- density(h1_plr)	
  medianaplr <- median(h1_plr)	
  cor_plr = "#ac4d12"	
  cor_mediana = "#000000"	
  cor_IC = "#0d5092"	
  maxx = max(d_plr$x,1)	
  maxy <- max(d_plr$y)	
  plot (d_plr, main=paste("Distribuicao de LR+",", n=",total,sep=""),	
        xlab = "LR+", ylab = "Densidade",	
        xlim = c(0,maxx), # ylim = c(0,maxy),	
        col=cor_OR, lwd=3, lty=1)	
  lines(	
    c(h1_ICplr[1],h1_ICplr[1],h1_ICplr[1],h1_ICplr[2],h1_ICplr[2],h1_ICplr[2]),	
    c(0,maxy/20,maxy/40,maxy/40,maxy/20,0),	
    col=cor_IC, lwd=2)	
  points(1,maxy/40,col="#000000", pch=19)	
  lines(c(medianaplr,medianaplr),c(0,maxy),col=cor_mediana,lty=2,lwd=1)	
  legend ("topleft",	
          c("LR+ simul.",	
            paste("mediana = ",round(medianaplr,5),sep=""),	
            paste("IC",round((1-alfa)*100,1),"%",sep=""),
            "referencia"
          ),	
          pch=c(NA,NA,NA,19),	
          lwd=c(3,1,2,NA),	
          lty=c(1,2,1,NA),	
          col=c(cor_plr,cor_mediana,cor_IC,"#000000"),	
          box.lwd=0,	
          bg="transparent")	
  
  #	nlr
  # distribuicao de nlr (LR-)	
  h1_nlr <- sort(h1_nlr)	
  h1_ICnlr <- quantile(h1_nlr,probs=c(alfa/2,1-(alfa/2)))	
  d_nlr <- density(h1_nlr)	
  mediananlr <- median(h1_nlr)	
  cor_nlr = "#ac4d12"	
  cor_mediana = "#000000"	
  cor_IC = "#0d5092"	
  maxx = max(d_nlr$x,1)	
  maxy <- max(d_nlr$y)	
  plot (d_nlr, main=paste("Distribuicao de LR-",", n=",total,sep=""),	
        xlab = "LR-", ylab = "Densidade",	
        xlim = c(0,maxx), # ylim = c(0,maxy),	
        col=cor_OR, lwd=3, lty=1)	
  lines(	
    c(h1_ICnlr[1],h1_ICnlr[1],h1_ICnlr[1],h1_ICnlr[2],h1_ICnlr[2],h1_ICnlr[2]),	
    c(0,maxy/20,maxy/40,maxy/40,maxy/20,0),	
    col=cor_IC, lwd=2)	
  points(1,maxy/40,col="#000000", pch=19)	
  lines(c(mediananlr,mediananlr),c(0,maxy),col=cor_mediana,lty=2,lwd=1)	
  legend ("topleft",	
          c("LR- simul.",	
            paste("mediana = ",round(mediananlr,5),sep=""),	
            paste("IC",round((1-alfa)*100,1),"%",sep=""),
            "referencia"
          ),	
          pch=c(NA,NA,NA,19),	
          lwd=c(3,1,2,NA),	
          lty=c(1,2,1,NA),	
          col=c(cor_nlr,cor_mediana,cor_IC,"#000000"),	
          box.lwd=0,	
          bg="transparent")	
  
  # distribuicao de p
  h0_p <- sort(h0_p)
  h0_ICp <- quantile(h0_p,probs=c(alfa/2,1-(alfa/2)))
  d_p_h0 <- density(h0_p)
  medianap_h0 <- median(h0_p)
  
  h1_p <- sort(h1_p)
  h1_ICp <- quantile(h1_p,probs=c(alfa/2,1-(alfa/2)))
  d_p_h1 <- density(h1_p)
  medianap_h1 <- median(h1_p)
  
  cor_h1 = "#EE8026"
  cor_h0 = "#507052"
  cor_mediana = "#000000"
  cor_IC = "#0d5092"
  
  maxx = max(d_p_h0$x,d_p_h1$x)
  maxy <- max(d_p_h0$y,d_p_h1$y)
  plot (d_p_h1, main=paste("Distribuicao do valor-p",", n=",total,sep=""),
        xlab = "p", ylab = "Densidade",
        xlim = c(0,maxx), ylim = c(0,maxy),
        col=cor_h1, lwd=3, lty=1
  )
  lines (d_p_h0, col=cor_h0, lwd=3, lty=2)
  # IC e mediana
  lines(c(h1_ICp[1],h1_ICp[1]),c(0,maxy/10),lty=1,lwd=2,col=cor_IC)
  lines(c(h1_ICp[2],h1_ICp[2]),c(0,maxy/10),lty=1,lwd=2,col=cor_IC)
  lines(c(h1_ICp[1],h1_ICp[2]),c(maxy/10/2,maxy/10/2),lty=1,lwd=2,col=cor_IC)
  lines(c(medianap_h1,medianap_h1),c(0,maxy),lty=2,lwd=1,col=cor_mediana)
  legend ("topright",
          c("H0","H1",
            "mediana H1",
            paste("IC(p|H1)",round((1-alfa)*100,1),"%",sep="")
          ), 
          pch=c(NA,NA,NA,NA), 
          lwd=c(3,3,1,1), 
          lty=c(2,1,2,1), 
          col=c(cor_h0,cor_h1,cor_mediana,cor_IC), 
          box.lwd=0,
          bg="transparent")
  
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
  
  titulo = paste(nomelin," x ",nomecol," (n=",total,", ",i," iteracoes)", sep="")
  # H0
  plot (dens_h0, main=titulo, 
        xlab="Valor da Estatistica Qui-Quadrado", 
        ylab="Densidade", 
        col=cor_H0, lwd=3,
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
  q95 <- quantile(h0_qui,probs=(1-medianap_h1), na.rm=TRUE)
  maxx <- mediah0+3*dph0
  if (maxx < as.vector(q95)) {maxx = as.vector(q95)}
  maxx <- maxx+1
  maxy <- max(dens_h0$y, dens_h1$y)
  
  # quantil 95% (empirico)
  q95 <- quantile(h0_qui,probs=1-alfa, na.rm=TRUE)
  
  cor_H0 = "#1965B0"
  cor_alfa_transparencia = paste(cor_H0,"88",sep="")
  cor_p = "#a30b1b"
  
  titulo = paste(nomelin," x ",nomecol," (n=",total,", ",i," iteracoes)", sep="")
  # H0
  plot (dens_h0, main=titulo, 
        xlab="Valor da Estatistica Qui-Quadrado", 
        ylab="Densidade", 
        col=cor_H0, lwd=3,
        xlim=c(truncar_x,maxx), ylim=c(0,maxy))
  # alfa
  q95 <- quantile(h0_qui,probs=1-alfa, na.rm=TRUE)
  d_alfa_x <- c(dens_h0$x[dens_h0$x>=q95]); 
  d_alfa_x <- c(min(d_alfa_x),d_alfa_x,max(d_alfa_x))
  d_alfa_y <- c(dens_h0$y[dens_h0$x>=q95])
  d_alfa_y <- c(0,d_alfa_y,0)
  polygon(d_alfa_x,d_alfa_y,col=cor_alfa_transparencia, border=cor_alfa_transparencia)
  # p
  q_h1 <- quantile(h0_qui,probs=(1-medianap_h1), na.rm=TRUE)
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
  
  # beta, contagem de h1 abaixo de q95 da h0
  beta_sml <- h1_qui[h1_qui < q95]
  beta_sml <- length(beta_sml)/num_iteracoes
  poder <- 1-beta_sml
  
  cat("\n\nTerminado:\n")
  cat("alfa: ",round(alfa*100,3),"%\n", sep="")
  cat("beta: ",round(beta_sml*100,3),"%\n", sep="")
  cat("poder do teste: ",round(poder*100,3),"%\n", sep="")
  
  cat("\nvalor-p:\n")
  cat("mediana(p):", medianap_h1,"\n")
  cat("IC",round((1-alfa)*100,1),"% (p): [",round(h1_ICp[1],3),", ",round(h1_ICp[2],3),"]\n", sep="")
  
  cat("\nOR:\n")
  cat("mediana(OR):", medianaOR,"\n")
  cat("IC",round((1-alfa)*100,1),"% (OR): [",round(h1_ICOR[1],3),", ",round(h1_ICOR[2],3),"]\n", sep="")
  
  cat("\nV de Cramer:\n")
  cat("mediana(V):", medianaV,"\n")
  cat("IC",round((1-alfa)*100,1),"% (V): [",round(h1_ICV[1],3),", ",round(h1_ICV[2],3),"]\n", sep="")

}

# libera outputs, caso usados
sink()
dev.off()

# enable warnings
options(warn=0)

if (!is.na(filename))
{
  cat(paste("\nVerifique os arquivos ",filetxt," e ",filepdf,sep=""))
  cat(paste("\ncom os resultados desta simulacao.\n",sep=""))
}
