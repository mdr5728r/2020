mes <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
         "Jul", "Ago", "Set", "Out", "Nov", "Dez")
mes <- ordered(mes, levels=mes)
high_NYC2014 <- c(28.8, 28.5, 37.0, 56.8, 69.7, 79.7, 78.5, 77.8, 74.1, 62.6, 45.3, 39.9)
low_NYC2014 <- c(12.7, 14.3, 18.6, 35.5, 49.9, 58.0, 60.0, 58.6, 51.7, 45.2, 32.2, 29.1)
plot(c(1:length(mes)), high_NYC2014, type="l", 
     ylim=c(0,max(high_NYC2014)),
     main="Temperaturas altas e baixas médias em NYC-2014", 
     xlab="Mes", ylab="Temperatura (F)", axes=FALSE)
axis(1, at=mes, labels=mes)
axis(2)
lines(mes, low_NYC2014, 
      main = "Temperaturas altas e baixas médias em NYC", 
      lty=2)
legend("topleft", c("High","Low"), lty=1:2)