# ModaEstatura.R

estatm <- c(169, 172, 176, 183, NA, 172, 181, 210, 172, 176, 176)

# moda discreta
modadiscreta <- function(x){w=table(x); w[max(w)==w]}
modad <- modadiscreta(estatm)
modas <- names(modad)
freqs <- as.vector(modad)
cat ("Moda(s) discreta(s) amostral(is): ",modas," com ",freqs[1]," ocorrencia(s)\n")
stripchart(estatm, method="stack", offset=0.5, at=0.15, pch=19)

# moda por densidade
d <- density(estatm,na.rm=TRUE)
moda <- d$x[which.max(d$y)]
cat("Moda continua amostral =",moda, "\n")
# grafico
plot(d)
lines(c(moda,moda), c(min(d$y),max(d$y)), lty=2)