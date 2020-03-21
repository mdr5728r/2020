mes <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
         "Jul", "Ago", "Set", "Out", "Nov", "Dez")
mes_num <- 1:length(mes)
high_NYC2014 <- c(28.8, 28.5, 37.0, 56.8, 69.7, 79.7, 78.5, 77.8, 74.1, 62.6, 45.3, 39.9)
low_NYC2014 <- c(12.7, 14.3, 18.6, 35.5, 49.9, 58.0, 60.0, 58.6, 51.7, 45.2, 32.2, 29.1)
dados <- data.frame(mes, high_NYC2014, low_NYC2014)
minx <- min(mes_num, na.rm=TRUE)
maxx <- max(mes_num, na.rm=TRUE)
miny <- min(high_NYC2014, low_NYC2014, na.rm=TRUE)
maxy <- max(high_NYC2014, low_NYC2014, na.rm=TRUE)
plot(NA, NA, 
     main = "Temperaturas altas e baixas médias em NYC-2014",
     xlim=c(minx,maxx), ylim=c(miny,maxy),
     xaxt="n", yaxt="n",
     xlab =  "Mes",
     ylab = "Temperatura (F)")
# x axis
axis(side=1, at=c(minx-1,maxx+1), labels = FALSE)
text(x=mes_num,  par("usr")[3], 
     labels = mes, srt = 30, pos = 1, xpd = TRUE)
# y axis
axis(side=2, las=1)
# high temps
high_cor <- "#333333" # cinza escuro 
high_lty <- 2  
lines(mes_num,high_NYC2014,lwd=2,lty=high_lty,col=high_cor)
points(mes_num,high_NYC2014,pch=25,col=high_cor,bg=high_cor)
# low temps
low_cor <- "#666666" # cinza medio
low_lty <- 3  
lines(mes_num,low_NYC2014,lwd=2,lty=low_lty,col=low_cor)
points(mes_num,low_NYC2014,pch=24,col=low_cor,bg=low_cor)
# high-low vertical lines
hl_x <- c()
hl_y <- c()
for (m in 1:length(mes))
{
  hl_x <- c(hl_x, mes_num[m],      mes_num[m]    , NA)
  hl_y <- c(hl_y, high_NYC2014[m], low_NYC2014[m], NA)
}
lines(hl_x,hl_y,lty=2, lwd=1,col="#888888")
# legenda
legend("topleft",
       c("Maximas", "Minimas"),
       col=c(high_cor,low_cor),
       pt.bg=c(high_cor,low_cor),
       pch=c(25,24),
       lwd=c(2,2),
       lty=c(high_lty,low_lty),
       box.lwd=0, bg="transparent")