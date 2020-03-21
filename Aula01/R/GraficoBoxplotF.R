estatura <- c(176, 183, 173, 191, 157, 152, 174, 166)
genero <- factor(c("M", "M", "M", "M", "F", "F", "F", "F"))
tabela <- data.frame(estatura, genero)
boxplot(estatura[genero=="F"], 
        main=paste("Boxplot\nN =", sum(is.na(estatura[genero=="F"]))),
        ylab="Estatura (cm)", 
        xlab="Feminino", 
        data=tabela)