source("friendlycolor.R")

estatura.masc <- rnorm(n=50, mean=177, sd=10)
estatdens <- density(estatura.masc)
plot(estatdens, 
     main="Grafico de densidade", 
     xlab="Estatura masculina (cm)", ylab="Densidade")
# caudas
limites <- quantile(estatura.masc, probs = c(0.025,0.975))
polx <- estatdens$x[estatdens$x<=limites[1]]
polx <- c(min(polx), polx, max(polx))
poly <- estatdens$y[estatdens$x<=limites[1]]
poly<- c(0, poly, 0)
polygon(polx,poly,border=NA,
        col=friendlycolor(8)
)
polx <- estatdens$x[estatdens$x>=limites[2]]
polx <- c(min(polx), polx, max(polx))
poly <- estatdens$y[estatdens$x>=limites[2]]
poly<- c(0, poly, 0)
polygon(polx,poly,border=NA,
        col=friendlycolor(8)
)
# transparencia (amarelo)
polx <- estatdens$x[estatdens$x>=190 & estatdens$x<=200]
polx <- c(min(polx), polx, max(polx))
poly <- estatdens$y[estatdens$x>=190 & estatdens$x<=200]
poly<- c(0, poly, 0)
polygon(polx,poly,border=NA,
        col=paste(friendlycolor(24),"88",sep="")
)
