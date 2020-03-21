library(BSDA)
estatura <- c(169, 174, 175, 186)
BSDA::z.test(x=estatura, sigma.x=7, mu = 177, 
             alternative="two.sided", conf.level=.95)
set.seed(3)
estatura <- rnorm(mean=176, sd=7, n=1000)
BSDA::z.test(x=estatura, sigma.x=7, mu = 177, 
             alternative="two.sided", conf.level=.95)
library(readxl)
Dados <- readxl::read_excel("Table 8.2 RawBirthWeight.xls")
BSDA::z.test(x=Dados$RBW, sigma.x=4, mu = 20, 
             alternative="less", conf.level=.95)
estatura <- c(169, 174, 175, 186)
BSDA::z.test(x=estatura, sigma.x=7, mu = 177, 
             alternative="less", conf.level=.95)
estatura <- c(169, 174, 175, 186)
BSDA::z.test(x=estatura, sigma.x=7, mu = 177, 
             alternative="greater", conf.level=.95)
library(EnvStats)
estatura <- c(169, 174, 175, 186)
out <- EnvStats::varTest(x=estatura, sigma.squared = 7^2,
        alternative="two.sided", conf.level = 0.95)
print(out)
print(sqrt(out$conf.int))
out <- EnvStats::varTest(x=estatura, sigma.squared = 7^2,
                         alternative="less", conf.level = 0.95)
print(out)
print(sqrt(out$conf.int))
