library(readxl)
library(mcr)
# x = PEFRmini
# y = PEFR
# Estimacao pontual do lambda para medidas repetidas usada na Regressao de Deming
# Chapter 303 do NCSS 11 (2016): Deming regression
PEFRDATA <- readxl::read_excel("PEFRDATA.xls")
PEFR.media <- (PEFRDATA$PEFR1 + PEFRDATA$PEFR2)/2
PEFR.mediag <- mean(PEFR.media, na.rm=TRUE)
PEFRmini.media <- (PEFRDATA$PEFRmini1 + PEFRDATA$PEFRmini2)/2
PEFRmini.mediag <- mean(PEFRmini.media, na.rm=TRUE)
n <- nrow(PEFRDATA)
VARdelta <- (sum((PEFRDATA$PEFR1 - PEFR.media)^2, na.rm=TRUE) + 
               sum((PEFRDATA$PEFR2 - PEFR.media)^2, na.rm=TRUE))/n
VARepsilon <- (sum((PEFRDATA$PEFRmini1 - PEFRmini.media)^2, na.rm=TRUE) + 
               sum((PEFRDATA$PEFRmini2 - PEFRmini.media)^2, na.rm=TRUE))/n
lambda_rm <- VARepsilon/VARdelta # lambda = 1.692
out_boot_rm <- mcr::mcreg(x=PEFRmini.media,y=PEFR.media,error.ratio=lambda_rm,
             method.reg="Deming", 
             method.ci="bootstrap", nsamples = 1e4, rng.seed = 123, 
             mref.name = "PEFR1mini", mtest.name = "PEFR1", na.rm=TRUE)
mcr::printSummary(out_boot_rm)
mcr::plot(out_boot_rm)
out_boot_rm1 <- mcr::mcreg(x=PEFRmini.media,y=PEFR.media,error.ratio=1,
                          method.reg="Deming", 
                          method.ci="bootstrap", nsamples = 1e4, rng.seed = 123, 
                          mref.name = "PEFR1mini", mtest.name = "PEFR1", na.rm=TRUE)
mcr::printSummary(out_boot_rm1)
mcr::plot(out_boot_rm1)
out_lr_rm <- mcr::mcreg(x=PEFRmini.media,y=PEFR.media,error.ratio=1,
                       method.reg="LinReg", 
                       method.ci="bootstrap", nsamples = 1e4, rng.seed = 123, 
                       mref.name = "PEFR1mini", mtest.name = "PEFR1", na.rm=TRUE)
mcr::printSummary(out_lr_rm)
mcr::plot(out_lr_rm)
mcr::compareFit(out_boot_rm, out_boot_rm1, out_lr_rm)


# Estimacao do IC95% do lambda sem medidas repetidas 
# Medida única ou para a média das repeticoes
# A ordem de x e y importa
# Shukla (1973), p. 376.
a <- var(PEFR.media) - cov(PEFR.media,PEFRmini.media)
b <- var(PEFRmini.media) - cov(PEFR.media,PEFRmini.media)
gl <- n - 2
alfa <- 0.05
t <- qt(1 - alfa/2, gl)
P <- (t^2)*(var(PEFR.media)*var(PEFRmini.media) - cov(PEFR.media,PEFRmini.media)^2)/(n - 2)
LI <- abs((b - sqrt(P))/(a + sqrt(P)))
LS <- abs((b + sqrt(P))/(a - sqrt(P)))
cat("IC95%(lambda) = [", round(LI,4), ",", round(LS,4),"]\n")
lambda <- (LI + LS)/2
cat("Estimativa pontual de lambda = ",round(lambda,4)) # lambda = 1.545

mcreg(x, y = NULL, error.ratio = 1, alpha = 0.05, mref.name = NULL,
      mtest.name = NULL, sample.names = NULL, method.reg = c("PaBa", "LinReg",
      "WLinReg", "Deming", "WDeming", "PaBaLarge"), method.ci = c("bootstrap",                                                                                                                         "jackknife", "analytical", "nestedbootstrap"),
      method.bootstrap.ci = c("quantile", "Student", "BCa", "tBoot"),
      nsamples = 999, nnested = 25, rng.seed = NULL,
      rng.kind = "Mersenne-Twister", iter.max = 30, threshold = 1e-06,
      na.rm = FALSE, NBins = 1e+06, slope.measure = c("radian", "tangent"))