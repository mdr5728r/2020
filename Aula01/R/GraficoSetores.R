faixaetaria <- sort(factor(c("Idoso", "Idoso", "Idoso", "Idoso", 
                             "Adulto", "Adulto", "Adulto", "Jovem")))
rotulo <- unique(faixaetaria)
freq <- summary(faixaetaria)
dt_tmp <- data.frame(c(2,3,1),rotulo,freq)
names(dt_tmp) <- c("ordem","rotulo","freq")
dt_tmp <- dt_tmp[order(dt_tmp$ordem),]
pie(dt_tmp$freq, 
    label = dt_tmp$rotulo,
    main = "Grafico de setores",
    xlab = "Faixa Etaria", 
    col = gray(seq(0.4,1.0,
                   length = length(dt_tmp$rotulo))))