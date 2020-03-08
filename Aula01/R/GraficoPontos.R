estatura <- as.integer(rnorm(n=50, mean=177, sd=10))
stripchart(estatura, method="stack", offset=0.5, at=0.15, pch=19, 
           xlab ="Estatura masculina (cm)", ylab ="Frequencia absoluta", 
           main="Dotplot")