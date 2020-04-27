# TesteQuiQuadradoAderencia_Chocolate.R

library(readxl)

cat("Teste de aderencia a distribuicao multinomial\n")

df_chocolate <- read_excel("chocolate.xlsx")

prmatrix(df_chocolate, rowlab = rep("",nrow(df_chocolate)))

out <- chisq.test(df_chocolate$Pessoas, p=df_chocolate$probH0, 
                  simulate.p.value = TRUE, B = 1e6) 
df <- chisq.test(df_chocolate$Pessoas, p=df_chocolate$probH0)$parameter
X2 <- out$statistic
p <- out$p.value
n <- sum(chisq.test(df_chocolate$Pessoas, p=df_chocolate$probH0)$observed)
V <- sqrt((X2/n)/df)
cat("X^2 =", X2 , "gl =",  df, "p exato =", p, "\n")
cat("\nHeuristica de significancia: X^2/gl > 2:\n", X2/df, "\n")
cat("\nResiduos estandardizados:\n", out$stdres, "\n")
if (0 <= V & V < 0.1) {gV <- "minimo"}
if (0.1 <= V & V < 0.3) {gV <- "pequeno"}
if (0.3 <= V & V < 0.5) {gV <- "intermediario"}
if (0.5 <= V & V <= 1.0) {gV <- "grande"}
cat("V de Cramer = ", V,"\n",sep="")
cat("(Grau ", gV, "de dessemelhanca entre as distribuicoes observada e hipotetizada\n",sep="")
