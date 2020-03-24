library(readxl)
library(BSDA)
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
PRRN.media.boot <- replicate(B, mean(sample(PRRN, replace=TRUE)))
print(mean(PRRN, na.rm=TRUE))
print(mean(PRRN.media.boot, na.rm=TRUE))
round(quantile(PRRN.media.boot, probs=1-alfa),2)
print(BSDA::z.test(x=Dados$RBW, sigma.x=4, mu = 20, 
                        alternative="less", conf.level=.95))
plot(density(PRRN.media.boot, na.rm=TRUE),
     main=paste("N =", N, "B =", B, 
                "\nErro-padrao da media amostral =", 
                round(sd(PRRN.media.boot, na.rm=TRUE), 4)), 
     xlab="Media amostral do PRRN (g)", ylab="Densidade")
mi <- mean(PRRN.media.boot, na.rm=TRUE)
EP <- sd(PRRN.media.boot, na.rm=TRUE)
x <- seq(from=mi-5*EP, to=mi+5*EP, by=1e-5)
y <- dnorm(x, mean=mi, sd=EP)
lines(x,y,lwd=2,lty=2)
legend("topright", c("Media amostral","Normal"), lty=1:2, cex=.6)

