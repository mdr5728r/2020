alfa <- 0.05
z <- c()
normal <- c()
range <- 5
h <- 0 # central
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
    gl <- c(1:30, seq(from=35, to=100, by=5), seq(from=120, to=300, by=20))
    d <- 2
  } else
  {
    Sys.sleep(3)
    gl <- c(seq(from=300, to=120, by=-20), seq(from=100, to=35, by=-5), 30:1)
    d <- 1
  }
  for (v in gl)
  {
    plot(z,normal, main="Normal Padronizada (z) vs. Distribuicao t",
         xlab="z ou t", ylab="Densidade",
         col="#F4A73688",
         lwd=8,
         type = "l")
    lines(c(licz,licz),c(0,0.05),col="#F4A736",lwd=1,lty=2)
    lines(c(uicz,uicz),c(0,0.05),col="#F4A736",lwd=1,lty=2)
    t <- c()
    dist_t <- c()     
    for (i in seq(from=-range, to=range, by=0.1))
    {
      t <- c(t,i)
      dist_t <- c(dist_t,dt(i, df=v, ncp=0))
    }
    lines (t,dist_t,col="#437DBF",lwd=2)
    lict <- qt(alfa/2, df=v, ncp=h)
    uict <- qt(1-alfa/2, df=v, ncp=h)
    print (paste("g.l. = ",v,", t critico = ",uict," vs. z critico = ",uicz,sep=""))
    altura_tc <- dt(lict, df=v, ncp=h)
    lines(c(lict,lict),c(0,altura_tc),col="#437DBF",lwd=1,lty=2)
    lines(c(uict,uict),c(0,altura_tc),col="#437DBF",lwd=1,lty=2)
    legend("topleft", 
           c("z", paste("t com ",v," g.l.",sep="")), 
           col=c("#F4A73688","#437DBF"),
           lwd=c(8,2), 
           lty=1, 
           box.lwd=0, bg="transparent")  
    
    # pausa
    tempo <- 0.5
    if (v < 15) {tempo <- 1}
    Sys.sleep(tempo)
  }
}


