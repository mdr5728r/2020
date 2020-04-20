# ANCOVA robusta a heterocedasticidade
# para delineamento pre-pos intervencao
library(readxl)
library(gplots)
library(car)
library(lsr)
library(emmeans)
library(multcomp)
library(estimatr)
library(ggplot2)
alfa <- 0.05
Dados <- readxl::read_excel("PrePos_2CondIndep.xlsx")
Dados$Grupo <- factor(Dados$Grupo,levels=unique(Dados$Grupo))
head(Dados)
with(Dados, gplots::plotmeans(Pos ~ Grupo, 
            error.bars="conf.int", level=1-alfa, connect=FALSE,
            xlab="Grupo", ylab="Pos", main="IC95%",
            barcol="black"))
Diferenca <- Dados$Pos - Dados$Pre
with(Dados, gplots::plotmeans(Diferenca ~ Grupo, 
            error.bars="conf.int", level=1-alfa, connect=FALSE,
            xlab="Grupo", ylab="Diferenca", main="IC95%",
            barcol="black"))
with(Dados, car::scatterplot(Pre, Pos, 
            groups = Grupo, 
            regLine=TRUE, smooth=FALSE, ellipse=FALSE, col="black", 
            xlab = "Pre", ylab = "Pos"))
print(ggplot(Dados, aes(y = Pos, x = Pre, group = Grupo, linetype = Grupo)) + 
  geom_point() + geom_smooth(method = "lm"))
# VD: Pos VI: Grupo
t.test(Pos ~ Grupo, data=Dados)
# VD: Pos - Pre VI: Grupo
t.test(Diferenca ~ Grupo, data=Dados)
# ANCOVA: VD: Pos com erro de mensuracao
#         VI: Grupo 
#         Covariavel: Pre sem erro de mensuracao
## ANCOVA: Teste de dissociacao entre fator e covariavel
cat("\nANCOVA: Teste de dissociacao entre fator e covariavel\n")
modelo <- lm(Pre ~ Grupo, data=Dados)
print(Anova <- car::Anova(modelo, type=3, white.adjust=TRUE))
## ANCOVA: Teste de igualdade das inclinacoes das retas de regressao
cat("\nANCOVA: Teste de igualdade das inclinacoes das retas de regressao\n")
modelo <- lm(Pos ~ Grupo + Pre + Grupo*Pre, data=Dados)
print(Anova <- car::Anova(modelo, type=3))
## ANCOVA: Teste do efeito do fator fixo: 
##          Se as declividades sao iguais, testar se os interceptos sao iguais.
cat("\nANCOVA: Teste do efeito do fator fixo\n")
cat("\tSe as declividades sao iguais, testar se os interceptos sao iguais.\n")
modelo <- lm(Pos ~ Grupo + Pre, data=Dados)
print(Anova <- car::Anova(modelo, type=3, white.adjust=TRUE))
print(lsr::etaSquared(modelo))
print(sumario <- summary(modelo))
print(EMM <- emmeans::emmeans(modelo, "Grupo"))
print(grf <- plot(EMM,  colors = "black", 
                  main="Estimated Marginal Means",
                  xlab="Pos controlado por Pre",
                  ylab="Grupo"))
mc <- multcomp::glht(modelo, linfct = mcp(Grupo = "Tukey"))
plot(mc,las=3)
# teste t relacionado por ANCOVA: 
#   VD: Pos com erro de mensuracao - Pre com erro de mensuracao
#   VI: Pre com erro de mensuracao - mean(Pre com erro de mensuracao)
# Fonte: HEDGERG, EC & AYRES, S (2015) The power of a paired t-test
#        whit a covariate. Soc Sci Res 50:277-91.
t.test(Dados$Pos, Dados$Pre, paired=TRUE)
with(Dados,
  print(summary(
  estimatr::lm_robust(Pos-Pre ~ 1 + I(Pre-mean(Pre,na.rm=TRUE)))))
)


