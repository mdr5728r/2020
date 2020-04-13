# demo_regressao.R

source("eiras.friendlycolor.R")

# valores
x <- 2:10
y <- c(1, 2.5, 2.4, 11, 5.8, 13.8, 14, 8.3, 13)
# para nao ter um ponto na coordenada do centroide
# (melhora o exemplo)
centro_x <- mean(x)
x[x==centro_x] <- x[x==centro_x]-0.5
cat("x:",x,"\n")
cat("y:",y,"\n")

# centroide (media de x, media de y)
centro_x <- mean(x)
centro_y <- mean(y)
# para nao ter um ponto na coordenada do centroide
x[x==centro_x] <- x[x==centro_x]-0.5
centro_x <- mean(x)

# scatterplot
plot(x, y,
     xlim = c(0,12), ylim = c(0,16),
     pch=21, col="black", bg=eiras.friendlycolor(24)
     )

# media de y, traca linha horizontal
y.H0 <- rep(centro_y,length(x))
lines (x, y.H0, col=eiras.friendlycolor(30), 
       lwd=3, lty=2)

# modelo linear, traca a reta
modelo <- lm(y ~ x)
intercepto <- modelo$coefficients[1]
inclinacao <- modelo$coefficients[2]
y.medio <- intercepto + inclinacao*x
lines (x, y.medio, col=eiras.friendlycolor(8), 
       lwd=3, lty=1)

# centroide
points(centro_x, centro_y, pch=21, col="black", bg="black")

# legenda
legend ("topleft",
        c("y observados","y médio",
          paste("reta: y_medio = ",round(intercepto,3)," + ",
                round(inclinacao,3),"x",sep="")
        ), 
        lwd=c(NA, 3, 3), 
        pch=c(21, NA, NA),
        col=c("black",eiras.friendlycolor(30), eiras.friendlycolor(8)), 
        pt.bg=c(eiras.friendlycolor(24),eiras.friendlycolor(30), eiras.friendlycolor(8)), 
        cex=0.8, box.lwd=0)

# parte explicada pelo modelo
for (i in 1:length(x))
{
  lines(c(x[i]-0.05,x[i]-0.05), c(y.medio[i],y.H0[i]), 
        lwd=2, lty=3, col=eiras.friendlycolor(8))
}

# resíduo
for (i in 1:length(x))
{
  lines(c(x[i]+0.05,x[i]+0.05), c(y[i],y.medio[i]), 
        lwd=2, lty=4, col=eiras.friendlycolor(25))
}

res <- summary(modelo)
print(res)
