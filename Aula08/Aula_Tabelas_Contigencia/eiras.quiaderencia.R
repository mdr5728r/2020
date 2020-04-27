# eiras.quiaderencia.R
# Teste Qui-quadrado de aderencia

eiras.quiaderencia <- function (df_dados, B=1e6)
{
  valores <- as.vector(unlist(df_dados[,1]))
  probabilidades <- as.vector(unlist(df_dados[,2]))
  # teste robusto (bootstrapping)
  robusto <- chisq.test(valores, p=probabilidades, 
                        simulate.p.value = TRUE, B = B) 
  # usando o teste convencional para obter os graus de liberdade
  tradicional <- chisq.test(valores, p=probabilidades) 
  df <- tradicional$parameter
  X2 <- robusto$statistic
  p <- robusto$p.value
  n <- sum(robusto$observed)
  V <- sqrt((X2/n)/df)
  # resultados  
  cat("----------------------------------------------------\n")
  cat("Teste de aderencia a distribuicao multinomial\n")
  prmatrix(df_dados, rowlab = rep("",nrow(df_dados)))
  cat("X^2 =", X2 , "gl =",  df, "p exato =", p, "\n")
  cat("\nHeuristica de significancia: X^2/gl > 2:\n", X2/df, "\n")
  cat("\nResiduos estandardizados:\n", robusto$stdres, "\n")
  if (0 <= V & V < 0.1) {gV <- "minimo"}
  if (0.1 <= V & V < 0.3) {gV <- "pequeno"}
  if (0.3 <= V & V < 0.5) {gV <- "intermediario"}
  if (0.5 <= V & V <= 1.0) {gV <- "grande"}
  cat("V de Cramer = ", V,"\n",sep="")
  cat("(Grau ", gV, " de dessemelhanca entre as distribuicoes observada e hipotetizada\n",sep="")
  cat("----------------------------------------------------\n")
}

