estatura.masc <- rnorm(n=50, mean=177, sd=10)
plot(density(estatura.masc), 
     main="Grafico de densidade", 
     xlab="Estatura masculina (cm)", ylab="Densidade")
rug(estatura.masc)