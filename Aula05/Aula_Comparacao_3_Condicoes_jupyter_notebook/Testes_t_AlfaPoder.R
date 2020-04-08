# Testes_t_AlfaPoder.R
# Ilustra o valor de alfa e 1-beta
# com numero crescente de testes 

source("friendlycolor.R")

cor_alfa <- friendlycolor(8)
cor_poder <- friendlycolor(20)

alfa_escolhido <- 0.05
beta_escolhido <- 0.20
alfa <- c()
poder <- c()
grupos <- c()
for (g in 2:20)
{
  grupos <- c(grupos,g)
  combinacoes <- choose(g,2)
  
  alfa <- c(alfa,1-(1-alfa_escolhido)^combinacoes)
  poder <- c(poder,(1-beta_escolhido)^combinacoes)
}
plot(grupos, poder,
     xlab="Número de grupos",
     ylab="Probabilidade",
     ylim=c(0,1),
     lwd=2, lty=2, col=cor_poder,
     type="b")
lines(grupos,alfa,
      lwd=2, lty=1, col=cor_alfa, 
      type="b")
legend("right",
       c("alfa", "poder"),
       col=c(cor_alfa,cor_poder),
       lwd=c(2,2),
       lty=c(1,2),
       box.lwd=0, bg="transparent")

