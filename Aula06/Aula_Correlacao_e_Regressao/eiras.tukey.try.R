# eiras.tukey.try.R
# dado um vetor de dados, rastreia transformacoes potencia de Tukey
# indo de -3 a +3 em passos de 0.5
# relata o valor que minimiza o coeficiente de assimetria de Pearson
# show = c("all","final","no")

source("eiras.density_and_normal.R")

eiras.tukey.try <- function(dados, xlab="dados", show="no")
{
  # transformacoes power = [-2.2]
  min_assim.Pearson <- NA
  min_potencia <- NA
  for (potencia in seq(from=-3, to=3, by=0.5))
  {
    potencia <- round(potencia,1)
    if(potencia<0)
    {
      dados_tukey <- -1/(dados^abs(potencia))
    }
    if (potencia==0)
    {
      dados_tukey <- log(dados)
    }
    if(potencia>0)
    {
      dados_tukey <- dados^potencia
    }
    dados_tukey <- dados_tukey[is.finite(dados_tukey)]
    if (show == "all")
    {
      cat(paste("\n------------------\nTukey power = ",potencia,":\n------------------\n",sep=""))
      if(potencia<0)
      {
        label_x <- paste("-1/(",xlab,")^",potencia,")",sep="")
        titulo <- paste("Transformacao potencia de Tukey -1/x^",potencia,sep="")    
      }
      if (potencia==0)
      {
        label_x <- paste("ln(",xlab,")",sep="")
        titulo <- paste("Transformacao potencia de Tukey ln(x)",sep="")    
      }
      if(potencia>0)
      {
        titulo <- paste("Transformacao potencia de Tukey x^",potencia,sep="")    
        label_x <- paste("(",xlab,")^",potencia,sep="")
        if (potencia==1)
        {
          titulo = "Dados originais"
          label_x <- xlab
        }
      }
    }
    # calcula
    media <- mean(dados_tukey, na.rm = TRUE)
    desvpad <- sd(dados_tukey, na.rm = TRUE)
    # quartis
    mediana <- median(dados_tukey, na.rm = TRUE)
    if (show == "all")
    {
      cat ("media:",media,"\n")
      cat ("desvio-padrao:",desvpad,"\n")
      cat ("mediana:",mediana,"\n")
    }
    # assimetria de Pearson
    assim.Pearson <- 3 * (media - mediana) / desvpad
    if (show == "all")
    {
      cat ("Coef. Assimetria de Pearson:",assim.Pearson,"\n")
    }
    if (is.na(min_assim.Pearson) || min_assim.Pearson > abs(assim.Pearson))
    {
      min_assim.Pearson <- abs(assim.Pearson)
      min_potencia <- potencia
    }
    
    # adiciona a assimetria
    if (show == "all")
    {
      titulo <- 
        paste(titulo,"\nAssimetria = ",round(assim.Pearson,4),sep="")  
      eiras.density_and_normal(dados_tukey, 
                               main=titulo,
                               col="black",
                               xlab=label_x, ylab="densidade")
    }
  }
  
  # relata o minimo encontrado
  if (show == "y" | show =="all")
  {
    cat("\n------------------\nMelhor ajuste\n------------------\n")
    if (min_potencia != 0)
    {
      cat("Tukey power = ",min_potencia,"\n")
    } else
    {
      cat("logarithm transformation\n")
    }
    cat ("Coef. Assimetria de Pearson:",min_assim.Pearson,"\n")
    # exibe o grafico
    if(min_potencia<0)
    {
      dados_tukey <- -1/(dados^abs(min_potencia))
      label_x <- paste("-1/(",xlab,")^",min_potencia,")",sep="")
      titulo <- paste("Transformacao potencia de Tukey -1/x^",min_potencia,sep="")    
    }
    if (min_potencia==0)
    {
      dados_tukey <- log(dados)
      label_x <- paste("ln(",xlab,")",sep="")
      titulo <- paste("Transformacao potencia de Tukey ln(x)",sep="")    
    }
    if(min_potencia>0)
    {
      dados_tukey <- dados^min_potencia
      titulo <- paste("Transformacao potencia de Tukey x^",min_potencia,sep="")    
      label_x <- paste("(",xlab,")^",",xlab,",sep="")
      if (min_potencia==1)
      {
        titulo = "Dados originais"
        label_x <- xlab
      }
    }
    dados_tukey <- dados_tukey[is.finite(dados_tukey)]
    titulo <- 
      paste(titulo,"\nAssimetria = ",round(assim.Pearson,4),sep="")  
    eiras.density_and_normal(dados_tukey, 
                             main=titulo,
                             col="black",
                             xlab=label_x, ylab="densidade")
    
  }
  return (min_potencia)
}
