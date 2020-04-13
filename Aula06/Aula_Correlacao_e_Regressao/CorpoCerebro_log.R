# Regressao linear univariada simples

# Caso queira fazer a previsão para um tamanho de corpo (NA para nao calcular) 
prever <- 6
prev_dec <- 2 # decimal places

# Tabela com a massa corporal total e cerebral medias de alguns animais.
# Objetivo: investigar que tipo de modelo poderia ser ajustado aos dados.
# Fonte: Relação entre massas cerebral e corporal: BUSSAB (1986, p. 92)
Tabela <- ("
Animal	          Corpo	Cerebro
Elefante.africano	6654	5712
Elefante.asiatico	2547	4603
Cavalo	          521	  655
Gorila	          207	  406
Burro	            187	  419
Jaguar	          100	  157
Humano            62	  1320
Chipanze	        52.2	440
Cabra	            27.7	115
Macaco.Rhesus	    6.8	  179
Porco.da.India	  1	    5.5
")

# regressão
dados <- read.table(textConnection(Tabela),header=TRUE)
print (dados)

dados$Cerebro.ln <- log(dados$Cerebro)
dados$Corpo.ln <- log(dados$Corpo)

if(!require(RcmdrMisc)){install.packages("RcmdrMisc")}
library("RcmdrMisc")
descritiva <- numSummary(dados[,c("Corpo.ln","Cerebro.ln")],
                         statistics=c("mean", "sd", "quantiles"), 
                         quantiles=c(0,.25,.5,.75,1))
cat("\n\nEstatística descritiva:\n")
print (descritiva)
detach("package:RcmdrMisc", unload=TRUE)

# scatterplot simples
min_y= min(dados$Cerebro.ln)
max_y= max(dados$Cerebro.ln)
plot(dados$Corpo.ln,dados$Cerebro.ln, xlab="Corpo (ln(kg))", ylab="Cerebro (ln(g))",
     ylim=c(min_y,max_y),
     main=NA,
     lwd=1
)
# variaveis dos eixos
dens <- density(dados$Corpo.ln)
plot (dens,xlab="Corpo (ln(kg))",ylab="densidade",main=NA)
dens <- density(dados$Cerebro.ln)
plot (dens,xlab="Cerebro (ln(g))",ylab="densidade",main=NA)

# regressao
rls <- lm(Cerebro.ln ~ 1 + Corpo.ln, data=dados)
regressao <- summary(rls)
cat("\n\nRegressao:\n")
print (regressao)
p.valor <- 1 - pf (
                  q=regressao$fstatistic[1], 
                  df1=regressao$fstatistic[2], 
                  df2=regressao$fstatistic[3] 
                  )
if (p.valor < 1e-4) 
{p.valor = "p < 0.0001"} else
{p.valor = paste("p = ",round(p.valor,4),sep="")}
# coeficientes e reta de regressao
b0 <- rls$coefficients[1]
b1 <- rls$coefficients[2]
lid <- min(dados$Corpo.ln,na.rm=TRUE)
lsd <- max(dados$Corpo.ln,na.rm=TRUE)
reg_x <- seq(from=lid, to=lsd, by=(lsd-lid)/100 )
reg_y <- b0+b1*reg_x
min_y <- min(reg_y,dados$Cerebro.ln)  
r.pearson <- regressao$r.squared**0.5
if (b1 < 0) {r.pearson <- -r.pearson}
plot(dados$Corpo.ln,dados$Cerebro.ln, 
     xlab="Corpo (ln(kg))", ylab="Cerebro (ln(g))",
     ylim=c(min_y,max_y),
     main=paste("ln(Cerebro)^ = ",round(b0,4)," + ",round(b1,4)," ln(Corpo)",
                "\nr^2 = ", round(regressao$r.squared,4),
                ", r = ", round(r.pearson,4),
                " (",p.valor,")",sep=""),
     lwd=1
)
lines(reg_x,reg_y,lty=1,lwd=2)
lines(lowess(dados$Corpo.ln,dados$Cerebro.ln), lty=2, col="#ff0000") # Robust locally weighted regression
# residuals
res_x <- c()
res_y <- c()
for (i in 1:length(dados$Corpo.ln))
{
  res_x <- c(res_x,dados$Corpo.ln[i],dados$Corpo.ln[i],NA)
  res_y <- c(res_y,dados$Cerebro.ln[i],b0+b1*dados$Corpo.ln[i],NA)
  # 
}
lines(res_x,res_y,cex=4,type="l",lty=3,col="#0000ff")

# Bagplot e highest posterior density region
library("aplpack")
bagplot(dados$Corpo.ln, dados$Cerebro.ln, main="Bagplot", 
        xlab="Corpo (ln(kg))", ylab="Cerebro (ln(g))",
        na.rm = TRUE)
if(!require(emdbook)){install.packages("emdbook")}
library("car")
influencePlot(rls, main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )
# confidenceEllipse(rls,levels=.95,Scheffe=FALSE,fill=TRUE,fill.alpha=0.1)

# Banda de confianca de 95% da reta populacional e
# Intervalo de predicao de 95% do valor da VD
# Functions to make SLR Confidence and Prediction Band Plots.
# Grant Brown, 2013 e Jose Siqueira, 2019
# Internal function used by ConfidenceBandPlot 
BasicRegressionPlot = function(X,Y, main = "", xlab = "",ylab = "", 
                               plt = TRUE, matchScale = FALSE)
{
  regression = lm(Y ~ X)
  lid <- min(X,na.rm=TRUE)
  lsd <- max(X,na.rm=TRUE)
  reg_x <- seq(from=lid, to=lsd, by=(lsd-lid)/100 )
  reg_y <- b0+b1*reg_x
  centr_x <- mean(X, na.rm=TRUE)
  centr_y <- mean(Y, na.rm=TRUE)
  if (plt)
  {
    minx <- min(X)
    maxx <- max(X)
    if (matchScale)
    {
      lower = min(min(X), min(Y)) - 0.2*max(sd(X), sd(Y))
      upper = max(max(X), max(Y)) + 0.2*max(sd(X), sd(Y))
      plot(X,Y,main = main,xlab=xlab,ylab=ylab,
           xlim = c(lower, upper), ylim = c(lower, upper),
           col="#dddddd") # para quase eliminar os pontos
    }
    else
    {
      plot(X,Y,main = main,xlab=xlab,ylab=ylab,
           xlim = c(min(X)-sd(X), max(X) + sd(X)),
           ylim = c(min(Y)-sd(Y), max(Y) + sd(Y)),
           col="#bbbbbb") # para quase eliminar os pontos
    }
    # regression line (solid for valid segment)
    abline(reg=regression, lty=2, col="#888888")
    lines(reg_x,reg_y,lty=1,lwd=2)
    # centroid
    points(centr_x, centr_y, cex = 1, pch=19)
  }
  return(regression)    
}
# Function used by ConfidenceBandPlot and PredictionBandPlot
BandPlot = function(X,Y,type = c("confidence", "prediction"),
                    main="RLS & Banda de confianca de 95%"
                    ,xlab="X",ylab="Y", col = "red", lty =2, lwd =1,
                    alpha=0.05, add = FALSE, matchScale = FALSE)
{
  if (! add)
  {
    regression = BasicRegressionPlot(X,Y,main,xlab,ylab,plt=TRUE,matchScale)
  }
  else
  {
    regression = BasicRegressionPlot(X,Y,plt=FALSE)
  }
  MSE = sum(regression$residuals^2)/regression$df.residual
  RSE = sqrt(MSE)
  n = length(Y)
  X0 = seq(min(X) - 10*sd(X), max(X) + 10*sd(X), length = 300)
  if (type == "confidence")
  {
    print("making 95% confidence band plot")
    SE = RSE*sqrt(1/n + ((X0 - mean(X))^2)/(sum((X-mean(X))^2)))
  }
  else if (type == "prediction")
  {
    print("making 95% prediction band plot")
    SE = RSE*sqrt(1 + 1/n + ((X0 - mean(X))^2)/(sum((X-mean(X))^2)))
  }
  else
  {
    print(paste("Unsupported type: ", type, sep = ""))
    print("Using Confidence Band")
  }
  # Banda de confianca de 95% de Working-Hotelling-Scheffe
  # Applied linear statistical model - Kutner et al - 5e - 2004, p. 63
  # Applied multivariate statistical analysis - Johnson & Wichern - 6e - 2007, p. 212
  # W2 = T2(p,n-1) = ((n-1)/(n-p))pF(p,n-p), p = 2 
  B = sqrt(((n-1)/(n-2))*2*qf(0.95,2,n-2))*SE
  Yhat = regression$coefficients[1] + regression$coefficients[2]*X0
  UB = Yhat + B
  LB = Yhat - B
  lines(X0, UB, lty=lty, col = col)
  lines(X0, LB, lty=lty, col = col)
}
# Make a Confidence Band Plot. The only required arguments are X and Y. 
ConfidenceBandPlot = function(X,Y,main="RLS & Banda de confianca de 95%\nde Working-Hotelling",xlab="X",
                              ylab="Y", col = "red", lty =2, lwd =1,matchScale = FALSE,
                              alpha=0.05, add = FALSE                             
)
{
  BandPlot(X,Y,type = "confidence", main = main, xlab =xlab,ylab=ylab,col=col,
           lty = lty, lwd = lwd, alpha = alpha, add = add, matchScale = matchScale)
}

# exibe o grafico
ConfidenceBandPlot(dados$Corpo.ln, dados$Cerebro.ln,
                   xlab="Corpo (ln(kg))", ylab="Cerebro (ln(g))",
)

# calculo da previsão para um tamanho de corpo
if (!is.na(prever))
{
  X <- dados$Corpo.ln
  Y <- dados$Cerebro.ln
  regression = lm(Y ~ X)
  MSE = sum(regression$residuals^2)/regression$df
  RSE = sqrt(MSE)
  n = length(X)
  SE = RSE*sqrt(1/n + ((prever - mean(X))^2)/(sum((X-mean(X))^2)))
  # Banda de confianca de 95% de Working-Hotelling-Scheffe
  # Applied linear statistical model - Kutner et al - 5e - 2004, p. 63
  # Applied multivariate statistical analysis - Johnson & Wichern - 6e - 2007, p. 212
  # W2 = T2(p,n-1) = ((n-1)/(n-p))pF(p,n-p), p = 2
  B = sqrt(((n-1)/(n-2))*2*qf(0.95,2,n-2))*SE
  Yhat = regression$coefficients[1] + regression$coefficients[2]*prever
  UB = Yhat + B
  LB = Yhat - B
  points(prever,Yhat)
  ic_x <- c(prever,prever)
  ic_y <- c(as.numeric(LB),as.numeric(UB))
  points (ic_x, ic_y, pch="-")
  lines (ic_x, ic_y, lwd=2)
  text (prever, LB-(UB-LB)/5,
        paste("Corpo:", prever, 
              "\nln(Cerebro)^ = ",round(Yhat,prev_dec),"\n IC95 = [",round(LB,prev_dec),", ",round(UB,prev_dec),"]",
              #            "\nTGRL^ = ",round(exp(Yhat),prev_dec),"\n IC95 = [",round(exp(LB),prev_dec),", ",round(exp(UB),prev_dec),"]",
              sep=""),
        col = "#000088", cex=0.8, pos=1
  )
}

# plot(gestantes$HT,gestantes$LEUC, xlab="HT (%)", ylab="LEUC (milhoes/mm3)")

cat("\n")
cat("#########################################################\n")
cat("Terminado\n")
cat("#########################################################\n")
