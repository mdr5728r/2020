# Populacao_e_EMP.R

foo <- function()
{
  source ("friendlycolor.R")
  
  # Uma variavel qualquer
  N <- 1000000 # tamanho da populacao
  mu1 <- 80 # media da populacao
  sigma1 <- 30 # desvio padrao da populacao
  n <- 100 # tamanho de cada amostra
  B <- 3000 # numero de amostras
  # set.seed(123)
  
  # criando uma populacao ficticia
  pop_valores <- round(rnorm(N, mean=mu1, sd=sigma1),0)
  mean_pop <- mean(pop_valores)
  sd_pop <- sd(pop_valores)
  # amostras
  n_amostral <- c(4,16,36)
  epm_amo <- sd_pop/(n_amostral ^0.5)
  x <- seq(from=mean_pop-4*sd_pop, 
           to=mean_pop+4*sd_pop, by=0.01)
  ymax <- max(dnorm(x, mean=mean_pop, sd=min(epm_amo)))
  dpop_valores <- density(pop_valores)
  plot (dpop_valores, 
        main=paste("Populacao fictícia e amostras\n",
                   "média = ",round(mean_pop,0),", d.p. = ",round(sd_pop,1),
                   sep=""), 
        xlab = "Valor", ylab = "densidade",
        xlim = c(mu1-4*sigma1,mu1+4*sigma1),
        ylim = c(0,max(dpop_valores$y,ymax)*1.1),
        col = friendlycolor(10),
        lwd=4, type = "l")
  tp <- 44
  for (i in -3:3)
  {
    lines(c(mean_pop-i*sd_pop, mean_pop),
          c(max(dpop_valores$y)*1.2,max(dpop_valores$y)*1.2),
          lwd=10, lty=1, col = paste(friendlycolor(8),tp,sep="") )
  }
  cor_idx <- 25
  cor_amo <- c()
  txt_amo <- c()
  for (a in 1:length(n_amostral))
  {
    mean_amo <- mean_pop
    epm_amo <- sd_pop/(n_amostral[a] ^0.5)
    x <- seq(from=mean_amo-4*epm_amo, 
             to=mean_amo+4*epm_amo, by=0.01)
    y <- dnorm(x, mean=mean_amo, sd=epm_amo)
    lines(x,y,col=friendlycolor(cor_idx),lwd=2,lty=a+1)
    txt_amo <- c(txt_amo,paste("n =",n_amostral[a],
                               ": N(",round(mean_amo,0),", ",round(epm_amo,1),")",
                               sep=""))
    cor_amo <- c(cor_amo,friendlycolor(cor_idx))
    cor_idx <- cor_idx+2
  }
  
  legend("right", 
         c("População", "Media +-3 d.p.",txt_amo), 
         col=c(friendlycolor(8),
               paste(friendlycolor(8),"44",sep=""),
               cor_amo),
         lwd=c(4,10,rep(2,length(txt_amo))), 
         lty=c(1,1,2:(length(txt_amo)+1)), 
         box.lwd=0, bg="transparent")  
}

foo()