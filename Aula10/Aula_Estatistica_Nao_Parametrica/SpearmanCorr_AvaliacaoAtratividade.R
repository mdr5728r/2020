library(haven)
library(car)
library(RVAideMemoire)
Dados <- haven::read_sav("Atratividade do outro e de si mesmo.sav")
plot(car::scatterplot(Dados$atrativoutro ~ Dados$atrativminha, 
                 regLine=FALSE, smooth=FALSE, boxplots=TRUE, 
                 jitter=list(x=0, y=0), col="black", data=Dados))
cor.test(Dados$atrativoutro, Dados$atrativminha,
         method = "spearm", exact=TRUE, na.rm=TRUE)
RVAideMemoire::spearman.ci(Dados$atrativoutro, Dados$atrativminha, 
                           nrep = 1e6)

# Comparison of several Spearman's rank correlation coefficients
set.seed(1510)
var1 <- c(1:15+rnorm(15,0,2),1:15+rnorm(15,0,2),1:15+rnorm(15,0,2))
var2 <- c(-1:-15+rnorm(15,0,2),1:15+rnorm(15,0,2),1:15+rnorm(15,0,2))
fact <- gl(3,15,labels=LETTERS[1:3])
Dados <- data.frame(var1, var2, fact)
RVAideMemoire::spearman.cor.multcomp(var1,var2,fact,nrep = 1e4)
# B and C similar but different from A

# Tests for (semi-)partial association/correlation between paired samples
set.seed(1444)
x <- 1:30
y <- 1:30+rnorm(30,0,2)
z1 <- runif(30,0,4)
z2 <- 30:1+rnorm(30,0,3)
RVAideMemoire::pcor.test(x,y,z1,method = "spearman",nrep = 1e4)
RVAideMemoire::pcor.test(x,y,list(z1,z2),method = "spearman",nrep = 1e4)
