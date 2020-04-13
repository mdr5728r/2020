library(psych)
library(DescTools)
alfa <- 0.05
N <- 500
r <- 0.5
# H0: ro = 0 vs. H1: ro != 0
# https://en.wikipedia.org/wiki/Pearson_correlation_coefficient
t <- r*sqrt((N-2)/(1-r^2))
p <- 2*pt(-abs(t), df=N-2)
cat("t(",N-2,") = ",t," , p = ",p,"\n",sep="")
DescTools::CorCI(r, n=N, conf.level = 1-alfa, alternative = "two.sided")
# H0: ro = 0 vs. H1: ro < 0
DescTools::CorCI(r, n=N, conf.level = 1-alfa, alternative = "less")
# H0: ro = 0 vs. H1: ro > 0
DescTools::CorCI(r, n=N, conf.level = 1-alfa, alternative = "greater")
# Duas correlacoes independentes
# Correlacao amostral entre salario inicial de atual de genero feminino = 0.759
# Correlacao amostral entre salario inicial de atual de genero masculino = 0.860
# As correlacoes populacionais são iguais para os dois generos?
psych::r.test(n=216, n2=258, 0.759, 0.86); cat("\n")
psych::r.test(n=216, n2=258, 0.759, 0.86)$z
psych::r.test(n=216, n2=258, 0.759, 0.86)$p
# Correlacao parcial
rXY <- 0.4
rXZ <- 0.85
rYZ <- 0.48
r.Parcial.Pearson <- (rXY - rXZ*rYZ)/(sqrt(1 - rXZ^2)*sqrt(1 - rYZ^2))
# Duas correlacoes dependentes
# 85 crianças do terceiro ano foram testadas com testes de inteligência (1), 
# habilidades aritméticas (2) e compreensão de leitura (3). 
# A correlação entre inteligência e habilidades aritméticas equivale a r12 = 0,53, 
# inteligência e leitura se correlacionam com r13 = 0,41 e 
# aritmética e leitura com r23 = 0,59. 
# A correlação entre inteligência e habilidade aritmética é diferente 
# da correlação entre inteligência e compreensão de leitura?
psych::r.test(n=85, r12=0.53, r13=0.41, r23=0.59); cat("\n")
psych::r.test(n=85, r12=0.53, r13=0.41, r23=0.59)$t
psych::r.test(n=85, r12=0.53, r13=0.41, r23=0.59)$p



