library(readxl)
library(EnvStats)
alfa <- 0.05
dp_0 <- 4
Dados <- readxl::read_excel("Table 8.2 RawBirthWeight.xls")
PRRN <- as.matrix(Dados[,2])
out <- EnvStats::varTest(x=PRRN[,1], sigma.squared = dp_0^2,
                         alternative="two.sided", conf.level = 0.95)
print(out)
print(sqrt(out$conf.int))
print(sqrt(out$estimate))
