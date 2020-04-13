# demo_r_de_Pearson.R

source("friendlycolor.R")

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
     pch=21, col="black", bg=friendlycolor(24)
     )

# media de y, traca linha horizontal
lines (x, rep(centro_y,length(x)), col=friendlycolor(30), 
       lwd=3, lty=2)

# modelo linear, traca a reta
modelo <- lm(y ~ x)
intercepto <- modelo$coefficients[1]
inclinacao <- modelo$coefficients[2]
y.estimado <- intercepto + inclinacao*x
lines (x, y.estimado, col=friendlycolor(8), 
       lwd=3, lty=1)

# legenda
legend ("topleft",
        c("y observados","y médio",
          paste("reta: y_medio = ",round(intercepto,3)," + ",
                round(inclinacao,3),"x",sep="")
        ), 
        lwd=c(NA, 3, 3), 
        pch=c(21, NA, NA),
        col=c("black",friendlycolor(30), friendlycolor(8)), 
        pt.bg=c(friendlycolor(24),friendlycolor(30), friendlycolor(8)), 
        cex=0.8, box.lwd=0)

# padronizando as variáveis x e y
centro_x <- mean(x)
desvpad_x <- sd(x)
z_x <- (x-centro_x)/desvpad_x
centro_y <- mean(y)
desvpad_y <- sd(y)
z_y <- (y-centro_y)/desvpad_y

cat("\nValores padronizados:\n")
cat("z_x:",round(z_x,3),"\n")
cat("z_y:",round(z_y,3),"\n")

cat("\nCorrelação com as variáveis padronizadas:\n")
z_correlacao <- cor.test(z_x, z_y)
print(z_correlacao)

# scatterplot
plot(z_x, z_y,
     xlim = c(-2,2), ylim = c(-2,2),
     pch=21, col="black", bg=friendlycolor(24)
)

# media de y, traca linha horizontal
z_centro_x <- mean(z_x)
z_centro_y <- mean(z_y)
lines (z_x, rep(z_centro_y,length(z_x)), col=friendlycolor(30), 
       lwd=3, lty=2)

# modelo linear, traca a reta
z_modelo <- lm(z_y ~ z_x)
z_intercepto <- z_modelo$coefficients[1]
z_inclinacao <- z_modelo$coefficients[2]
z_y.estimado <- z_intercepto + z_inclinacao*z_x
lines (z_x, z_y.estimado, col=friendlycolor(8), 
       lwd=3, lty=1)
# centroide
points(z_centro_x, z_centro_y, pch=21, col="black", bg="black")

# legenda
legend ("topleft",
        c("y obs. pad.","y médio pad.",
          paste("reta: y_medio = ",round(z_intercepto,3)," + ",
                round(z_inclinacao,3),"x",sep="")
        ), 
        lwd=c(NA, 3, 3), 
        pch=c(21, NA, NA),
        col=c("black",friendlycolor(30), friendlycolor(8)), 
        pt.bg=c(friendlycolor(24),friendlycolor(30), friendlycolor(8)), 
        cex=0.8, box.lwd=0)

# parte explicada pelo modelo
for (i in 1:length(z_x))
{
  lines(c(z_x[i],z_x[i]), c(z_y[i],z_y.estimado[i]), 
        lwd=2, lty=3)
}