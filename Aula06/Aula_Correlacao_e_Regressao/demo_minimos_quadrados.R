# demo_minimos_quadrados.R
# faz uma animacao mostrando porque a reta ajustada
# eh a que obtem a menor area dos quadrados 

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

# modelo linear, traca a reta
modelo <- lm(y ~ x)
intercepto <- modelo$coefficients[1]
inclinacao <- modelo$coefficients[2]
y.estimado <- intercepto + inclinacao*x
lines (x, y.estimado, col=friendlycolor(8), 
       lwd=3, lty=1)

range_x <- abs(max(x)-min(x))
min_x <- min(x)-range_x
max_x <- max(x)+range_x
range_y <- abs(max(y)-min(y))
min_y <- min(y)-range_y
max_y <- max(y)+range_y
# grafico quadrado
min_x <- min_y <- min(min_x,min_y)
max_x <- max_y <- max(max_x,max_y)

# y = b0 + b1x
b0 <- seq(from=centro_y-2*range_y, to=centro_y+2*range_y, by=0.5)
# adiciona o intercepto da melhor reta
modelo <- lm(y ~ x)
intercepto <- modelo$coefficients[1]
b0 <- unique(sort(c(b0,as.numeric(intercepto))))

# anima retas ao redor do centroide
b_inc <- 1
i <- 0
while (1) # loop eterno
{
  i <- i + b_inc
  # muda o valor de b0, altera a direcao do incremento nos extremos
  if (i < 1) {i <- 1; b_inc <- 1;}
  if (i > length(b0)) {i <- length(b0); b_inc <- -1;}
  
  # b1 <- (y - b0)/x
  b1 <- (centro_y - b0[i]) / centro_x
  y_calc <- b0[i] + b1 * x
  
  # scatterplot
  plot(x, y,
       main="Demonstracao: metodo dos minimos quadrados",
       xlim = c(min_x,max_x), 
       ylim = c(min_y,max_y),
       pch=21, col="black", bg=friendlycolor(24)
  )
  # reta
  color <- "#000000"
  aviso_min <- ""
  if (b0[i]==intercepto) 
  {  
    color <- friendlycolor(8)
    aviso_min <- "(area minima)"
  }
  lines(x, y_calc, col=color, lwd=3, lty=1)
  # centroide
  points(centro_x,centro_y,pch=21,col="black",bg="black")
  
  # conecta cada valor observado com a reta
  somaquadrados <- 0
  for (ix in 1:length(x))
  {
    lado <- abs(y_calc[ix]-y[ix])
    somaquadrados <- somaquadrados+lado^2
    x_vertice <- c(x[ix],x[ix],x[ix]+lado,x[ix]+lado,x[ix])
    y_vertice <- c(y[ix],y_calc[ix],y_calc[ix],y[ix],y[ix])
    polygon(x_vertice,y_vertice,
            border=NA, col=paste(color,"44",sep=""))
    lines(x_vertice,y_vertice,
          lwd=1, lty=2)
  }
  text(centro_x, max_y, 
       paste("soma dos quadrados = ",
             sprintf("%.3f",somaquadrados), sep="")
  )
  if (length(aviso_min) > 0) 
  {text(centro_x, max_y*0.95, aviso_min)}
  # pausa
  if (b0[i]==intercepto) {Sys.sleep(2)}
  {Sys.sleep(0.1)}
}

