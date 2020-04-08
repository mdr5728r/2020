alfa <- 0.05
z <- c()
normal <- c()
range <- 15
h <- 0 # central
old_h <- -1
for (i in seq(from=-range, to=range, by=0.1))
{
  z <- c(z,i)
  normal <- c(normal,dnorm(i, mean=0, sd=1))
}
licz <- qnorm(alfa/2, mean=0, sd=1)
uicz <- qnorm(1-alfa/2, mean=0, sd=1)

d <- 1 # direcao
while (1)
{
  if (d==1)
  {
    print ("---------------------------------------------------------")
    print ("Mostra distribuicoes t em loop: interrompa quando quiser.")
    gl <- c(1:10, seq(from=20, to=100, by=20), seq(from=150, to=200, by=50))
    d <- 2
  } else
  {
    Sys.sleep(3)
    gl <- c(seq(from=200, to=150, by=-50), seq(from=100, to=20, by=-20), 10:1)
    d <- 1
  }
  for (v in gl)
  {
    t <- c()
    dist_t <- c()     
    for (i in seq(from=-range, to=range, by=0.1))
    {
      t <- c(t,i)
      dist_t <- c(dist_t,dt(i, df=v, ncp=0))
    }
    lict <- qt(alfa/2, df=v, ncp=0)
    uict <- qt(1-alfa/2, df=v, ncp=0)
    hl <- c(seq(from=0.5,to=10,by=0.5),
            seq(from=10,to=0.5,by=-0.5),
            seq(from=-0.5,to=-10,by=-0.5),
            seq(from=-10,to=-0.5,by=0.5))

    for (h in hl)
    {
      # exibe t critico quando muda de sinal
      if (h > 0 & old_h < 0)
      {
        print (paste("g.l. = ",v,", t critico = ",uict," vs. z critico = ",uicz,sep=""))
      }
      if (h < 0 & old_h > 0)
      {
        print (paste("g.l. = ",v,", t critico = ",lict," vs. z critico = ",licz,sep=""))
      }
      old_h <- h
      plot(z,normal, main="Normal Padronizada (z) vs. Distribuicao t",
           xlab="z ou t", ylab="Densidade",
           col="#F4A73688",
           lwd=8,
           type = "l")
      lines(c(licz,licz),c(0,0.05),col="#F4A736",lwd=1,lty=2)
      lines(c(uicz,uicz),c(0,0.05),col="#F4A736",lwd=1,lty=2)
      dist_tncp <- c()     
      for (i in seq(from=-range, to=range, by=0.1))
      {
        dist_tncp <- c(dist_tncp,dt(i, df=v, ncp=h))
      }
      linha = 2
      cor = "#437DBF"
      if (h != 0) {linha = 8; cor="#437DBF55";}
      # t centrada em zero
      lines (t,dist_t,col=cor,lwd=linha)
      # t nao centrada
      lines (t,dist_tncp,col="#26a169",lwd=2)
      if (h >= 0)
      {
        altura_tc <- dt(uict, df=v, ncp=h)
        if (altura_tc < 0.01) {altura_tc = 0.01}
        lines(c(uict,uict),c(0,altura_tc),col="#437DBF",lwd=1,lty=2)
        posleg <- "topleft"
      }
      if (h <= 0)
      {
        altura_tc <- dt(lict, df=v, ncp=h)
        if (altura_tc < 0.01) {altura_tc = 0.01}
        lines(c(lict,lict),c(0,altura_tc),col="#437DBF",lwd=1,lty=2)
        posleg <- "topright"
      }
      legend(posleg, 
             c("z", 
               paste("t com ",v," g.l.",", centrada",sep=""),
               paste("t com ",v," g.l.",", ncp=",h,sep="")
             ), 
             col=c("#F4A73688",cor,"#26a169"),
             lwd=c(8,linha,2), 
             lty=1, 
             box.lwd=0, bg="transparent")  
      
      # pausa
      tempo <- 0.1
      Sys.sleep(tempo)
    }
  }
}


