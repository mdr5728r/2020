estatura <- c(176, 183, 173, 191, 157, 152, 174, 166)
genero <- factor(c("M", "M", "M", "M", "F", "F", "F", "F"))
tabela <- data.frame(estatura, genero)
boxplot(estatura ~ genero, 
        main=paste("Boxplot\nN_Fem =", sum(!is.na(estatura[genero=="F"])), 
                   ", N_Masc =", sum(!is.na(estatura[genero=="M"]))),
        ylab="Estatura (cm)",
        xlab="Genero",
        data=tabela)