# friendlydemo.R

source("friendlycolor.R")
indice <- 1
plot(NA, xlim=c(0,10), ylim=c(0,7))
for (x in 1:8)
{
  for (y in 1:6)
  {
    if (indice > 46) {next}
    
    points(x, 7-y, pch=21, cex=3, col="black", bg=friendlycolor(indice))
    cortexto <- "black"
    if (indice >= 31 & indice <= 36)
    {
      cortexto <- "white"
    }
    text(x, 7-y, indice, col=cortexto)
    indice <- indice+1
  }
}
