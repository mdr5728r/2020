library(readxl)
library(EnvStats)
B <- 1e6; alfa <- 0.05; set.seed(123)
Dados <- readxl::read_excel("Table 8.2 RawBirthWeight.xls")
PRRN <- as.matrix(Dados[,2])
N <- nrow(PRRN)
plot(density(PRRN, na.rm=TRUE),
     main=paste("N =", N, 
                "Media =", round(mean(PRRN,na.rm=TRUE),1), 
                "Desvio-padrao =", 
                round(sd(PRRN, na.rm=TRUE), 4)), 
     xlab="Peso do rato recem-nascido (g)", ylab="Densidade")
PRRN.dp.boot <- replicate(B, sd(sample(PRRN, replace=TRUE)))
print(dp <- sd(PRRN, na.rm=TRUE))
print(mean(PRRN.dp.boot, na.rm=TRUE))
quantile(PRRN.dp.boot, probs=c(alfa/2, 1 - alfa/2))
ICDP <- EnvStats::varTest(PRRN[,1])
plot(density(PRRN.dp.boot, na.rm=TRUE),
     main=paste("N =", N, "B =", B, 
                "\nErro-padrao do desvio-padrao amostral =", 
                round(sd(PRRN.dp.boot, na.rm=TRUE), 4)), 
     xlab="Desvio-padrao amostral do PRRN (g)", ylab="Densidade")
mi <- mean(PRRN.dp.boot, na.rm=TRUE)
print(EP <- sd(PRRN.dp.boot, na.rm=TRUE))
x <- seq(from=mi-5*EP, to=mi+5*EP, by=1e-5)
y <- dnorm(x, mean=mi, sd=EP)
lines(x,y,lwd=2,lty=2)
legend("topright", c("Desvio-padrao amostral","Normal"), lty=1:2, cex=.6)

