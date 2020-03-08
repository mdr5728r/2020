library(RcmdrMisc)
estatura <- c(176, 183, 173, 191, 157, 152, 174, 166)
genero <- factor(c("M", "M", "M", "M", "F", "F", "F", "F"))
tabela <- data.frame(estatura, genero)
with(tabela, plotMeans(estatura, genero, error.bars="conf.int", level=0.95, 
                       xlab="Genero", ylab="Estatura", main="IC95%", 
                       connect=FALSE))