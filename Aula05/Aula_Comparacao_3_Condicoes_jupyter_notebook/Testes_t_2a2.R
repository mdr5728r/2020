# Testes_t_2a2.R
# Ilustra o numero de testes t necessarios
# para comparar grupos par a par

combinacoes <- c()
grupos <- c()
for (g in 2:20)
{
  grupos <- c(grupos,g)
  combinacoes <- c(combinacoes,choose(g,2))
}
plot(grupos,combinacoes,
     xlab="Número de grupos",
     ylab="Número de testes t feitos par a par",
     lwd=2,
     type="b")
