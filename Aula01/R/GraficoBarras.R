faixaetaria <- sort(factor(c("Idoso","Idoso","Idoso","Idoso",
                             "Adulto","Adulto","Adulto","Jovem")))
rotulo <- unique(faixaetaria)
freq <- summary(faixaetaria)
dt_tmp <- data.frame(c(2,3,1),rotulo,freq)
names(dt_tmp) <- c("ordem","rotulo","freq")
dt_tmp <- dt_tmp[order(dt_tmp$ordem),]
barplot(dt_tmp$freq,
        main = "Grafico de barras",
        xlab = "Faixa Etaria", ylab = "Frequencia Absoluta",
        names.arg = dt_tmp$rotulo,
        col = gray(seq(0.4,1.0,length = length(dt_tmp$rotulo))))