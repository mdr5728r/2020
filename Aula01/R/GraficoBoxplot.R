estatura <- c(176, 183, 173, 191, 157, 152, 174, 266)
genero <- factor(c("M", "M", "M", "M", "F", "F", "F", "F"))
tabela <- data.frame(estatura, genero)
boxplot(estatura, 
        main=paste("Boxplot\nN =", sum(!is.na(estatura))),
        ylab="Estatura (cm)",
        data=tabela)