alfa <- 0.05
z <- c()
range <- 30
h <- 0 # central
old_h <- -1

acm_h <- c()
acm_v1 <- c()
acm_v2 <- c()
acm_fc <- c()
acm_poder <- c()
acm_eta2 <- c()

# pdf("Experimento_Fobs_Poder_Eta2.pdf")

d <- 1 # direcao
while (1)
{
  if (d==1)
  {
    print ("---------------------------------------------------------")
    print ("Mostra distribuicoes F em loop: interrompa quando quiser.")
    gl1 <- c(2:5)
    gl2 <- c(seq(from=range, to=range+80, by=40))
    d <- 2
  } else
  {
    Sys.sleep(3)
    gl1 <- c(5:2)
    gl2 <- c(seq(from=range+80, to=range, by=-40))
    d <- 1
  }
  for (v1 in gl1)
  {
    if (v1 == 0) {v1 <- 1}
    for (v2 in gl2)
    {
      # valor critico, dado alfa
      uicF <- qf(1-alfa/2, df1=v1, df2=v2, ncp=0)
      print (paste("F (",v1,", ",v2,"), F critico = ",round(uicF,3),sep=""))
      # computa a distribuicao sob H0
      F <- c()
      dist_F <- c()     
      F_alfa <- c()
      dist_F_alfa <- c()     
      for (i in seq(from=0, to=range, by=0.1))
      {
        # valor F
        F <- c(F,i)
        # densidade
        dens <- df(i, df1=v1, df2=v2, ncp=0)
        dist_F <- c(dist_F,dens)
        # segmento acima de F critico sob H0
        if (i>=uicF) 
        {
          F_alfa <- c(F_alfa,i)
          dist_F_alfa <- c(dist_F_alfa,dens)
        } 
      }
      
      # computa as distribuicoes sob H1
      # hl <- c(seq(from=0.5,to=range-5,by=0.05),
      #         seq(from=range-5,to=0.5,by=-0.05))
      hl <- c(seq(from=0.5,to=uicF-0.25,by=0.2),
              seq(from=uicF-0.25,to=uicF+0.25,by=0.1),
              seq(from=uicF+0.25,to=range/4,by=0.4),
              seq(from=range/4,to=range-5,by=0.6),
              seq(from=range-5,to=range/4,by=-0.6),
              seq(from=range/4,to=uicF+0.25,by=-0.4),
              seq(from=uicF+0.25,to=uicF-0.25,by=-0.1),
              seq(from=uicF-0.25,to=0.5,by=-0.2)
      )
      for (h in hl)
      {
        # parametro de nao-centralidade
        eta2 <- v1*h/(v1*h+v2)
        f2 <- eta2/(1-eta2)
        ncp <- v2*f2 
        beta <- pf(uicF, df1=v1, df2=v2, ncp)
        
        acm_v1 <- c(acm_v1,v1)
        acm_v2 <- c(acm_v2,v2)
        acm_h <- c(acm_h,h)
        acm_fc <- c(acm_fc,uicF)
        acm_poder <- c(acm_poder,1-beta)
        acm_eta2 <- c(acm_eta2,eta2)
        
        # computa a distribuicao sob H1 no mesmo range de H0
        dist_Fncp <- c()
        Fncp_beta <- c()
        dist_Fncp_beta <- c()     
        # posicao de F observado
        fobs_x <- h
        fobs_y <- df(h, df1=v1, df2=v2, ncp=ncp)
        for (i in F)
        {
          dens <- df(i, df1=v1, df2=v2, ncp=ncp)
          dist_Fncp <- c(dist_Fncp,dens)
          # segmento abaixo de F critico sob H1
          if (i <= uicF) 
          {
            Fncp_beta <- c(Fncp_beta,i)
            dist_Fncp_beta <- c(dist_Fncp_beta,dens)
          }
        }
        # F sob H0
        plot(F,dist_F, main="Distribuicao F",
             xlab="F", ylab="Densidade",
             ylim=c(0,1),
             col="#437DBFaa",
             lwd=4,
             type = "l")
        # F sob H1, ncp > 0
        lines (F,dist_Fncp,col="#26a169",lwd=4)
        # F critico
        abline(v=uicF,col="#F4A736",lwd=2,lty=2)
        # hachura alfa e beta
        F_alfa <- c(min(F_alfa),F_alfa,max(F_alfa))
        dist_F_alfa <- c(0,dist_F_alfa,0)
        polygon(F_alfa,dist_F_alfa,col="#BA8DB488", border="#BA8DB4")
        Fncp_beta <- c(min(Fncp_beta),Fncp_beta,max(Fncp_beta))
        dist_Fncp_beta <- c(0,dist_Fncp_beta,0)
        polygon(Fncp_beta,dist_Fncp_beta,col="#f4332888", border="#f43328")
        # seta, posicao de H1
        arrows(fobs_x,1,fobs_x,fobs_y+0.05,length=0.15,angle=10,lwd=1)
        # legenda
        posleg <- "topright"
        legend(posleg,
               c(paste("H0: F (",v1,", ",v2,")",", ncp=0", sep=""),
                 paste("H1: F (",v1,", ",v2,"), ncp=",format(round(ncp,1), nsmall = 1),sep=""),
                 paste("F_c=",format(round(uicF,3),nsmall = 3),sep=""),
                 paste("F_obs=",format(round(h,3),nsmall=3),sep=""),
                 paste("alfa=",round(alfa*100,0),"%",sep=""),
                 paste("beta=",round(beta*100,0),"%",sep="")
               ),
               col=c("#437DBFaa","#26a169","#F4A736","#000000","#BA8DB488","#f4332888"),
               lwd=c(4,4,2,1,5,5),
               lty=c(1,1,2,1,1,1),
               box.lwd=0, bg="transparent")
        
        # pausa
        tempo <- 0.1
        Sys.sleep(tempo)
      }
      
      acm_v1 <- c(acm_v1,NA)
      acm_v2 <- c(acm_v2,NA)
      acm_fc <- c(acm_fc,NA)
      acm_h <- c(acm_h,NA)
      acm_poder <- c(acm_poder,NA)
      acm_eta2 <- c(acm_eta2,NA)
      
    }
    plot(acm_h[acm_v1==v1],acm_poder[acm_v1==v1], type='l',lty=2, main=paste("v1 =",v1))
    for (i in 2:length(acm_h)) 
    {
      if (is.na(acm_h[i])) {next} 
      if (acm_v1[i]!=v1) {next}
      if (acm_h[i]<=acm_fc[i])
      { 
        lines(c(acm_h[i-1],acm_h[i]), c(acm_poder[i-1],acm_poder[i]),lwd=3,col="blue") 
      } 
    }    
    plot(acm_h[acm_v1==v1],acm_eta2[acm_v1==v1], type='l',lty=2, main=paste("v1 =",v1))
    for (i in 2:length(acm_h)) 
    {
      if (is.na(acm_h[i])) {next} 
      if (acm_v1[i]!=v1) {next}
      if (acm_h[i]<=acm_fc[i])
      { 
        lines(c(acm_h[i-1],acm_h[i]), c(acm_eta2[i-1],acm_eta2[i]),lwd=3,col="blue") 
      } 
    }    
    plot(acm_poder[acm_v1==v1],acm_eta2[acm_v1==v1], type='l',lty=2, main=paste("v1 =",v1))
    for (i in 2:length(acm_h)) 
    {
      if (is.na(acm_h[i])) {next} 
      if (acm_v1[i]!=v1) {next}
      if (acm_h[i]<=acm_fc[i])
      { 
        lines(c(acm_poder[i-1],acm_poder[i]), c(acm_eta2[i-1],acm_eta2[i]),lwd=3,col="blue") 
      } 
    }    
  }
  break
}

dev.off()
