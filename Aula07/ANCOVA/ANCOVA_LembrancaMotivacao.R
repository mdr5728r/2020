library(readxl)
library(gplots)
library(car)
library(lsr)
library(emmeans)
library(multcomp)
library(ggplot2)
alfa <- 0.05
Dados <- readxl::read_excel("LembrancaMotivacao.xlsx")
Dados$Grupo <- factor(Dados$Grupo,levels=unique(Dados$Grupo))
head(Dados)
with(Dados, gplots::plotmeans(Escore ~ Grupo, 
            error.bars="conf.int", level=1-alfa, connect=FALSE,
            xlab="Grupo", ylab="Escore", main="IC95%",
            barcol="black"))
with(Dados, car::scatterplot(Motivacao, Escore, 
                             groups = Grupo, 
            regLine=TRUE, smooth=FALSE, ellipse=FALSE, col="black", 
            xlab = "Motivacao ", ylab = "Escore "))
print(ggplot(Dados, aes(y = Motivacao, x = Escore, 
                    group = Grupo, linetype = Grupo)) + 
                    geom_point() + geom_smooth(method = "lm"))
# VD: Escore VI: Grupo
modelo <- lm(Escore ~ Grupo, data=Dados)
print(Anova <- car::Anova(modelo, type=3, white.adjust=TRUE))
print(lsr::etaSquared(modelo))
print(sumario <- summary(modelo))
print(EMM <- emmeans::emmeans(modelo, "Grupo"))
print(grf <- plot(EMM,  colors = "black", 
                  main="Estimated Marginal Means",
                  xlab="Escore",
                  ylab="Grupo"))
mc <- multcomp::glht(modelo, linfct = mcp(Grupo = "Tukey"))
print(mcs <- summary(mc, test=adjusted("bonferroni")))
multcomp::cld(mcs, level=alfa, decreasing=TRUE)
plot(mc,las=3)
# VD: Escore - Motivacao VI: Grupo
Diferenca <- Dados$Escore - Dados$Motivacao
modelo <- lm(Diferenca ~ Grupo, data=Dados)
print(Anova <- car::Anova(modelo, type=3, white.adjust=TRUE))
print(lsr::etaSquared(modelo))
print(sumario <- summary(modelo))
print(EMM <- emmeans::emmeans(modelo, "Grupo"))
print(grf <- plot(EMM,  colors = "black", 
                  main="Estimated Marginal Means",
                  xlab="Escore - Motivacao",
                  ylab="Grupo"))
mc <- multcomp::glht(modelo, linfct = mcp(Grupo = "Tukey"))
print(mcs <- summary(mc, test=adjusted("bonferroni")))
multcomp::cld(mcs, level=alfa, decreasing=TRUE)
plot(mc,las=3)
## ANCOVA: Teste de dissociacao entre fator e covariavel
cat("\nANCOVA: Teste de dissociacao entre fator e covariavel\n")
modelo <- lm(Motivacao ~ Grupo, data=Dados)
print(Anova <- car::Anova(modelo, type=3, white.adjust=TRUE))
## ANCOVA: Teste de igualdade das inclinacoes das retas de regressao
cat("\nANCOVA: Teste de igualdade das inclinacoes das retas de regressao\n")
modelo <- lm(Escore ~ Grupo + Motivacao + Grupo*Motivacao, 
             data=Dados)
print(Anova <- car::Anova(modelo, type=3))
## ANCOVA: Teste do efeito do fator fixo: 
##          Se as declividades sao iguais, testar se os interceptos sao iguais.
cat("\nANCOVA: Teste do efeito do fator fixo:\n")
cat("\tSe as declividades sao iguais, testar se os interceptos sao iguais.\n")
modelo <- lm(Escore ~ Grupo + Motivacao, 
             data=Dados)
print(Anova <- car::Anova(modelo, type=3, white.adjust=TRUE))
print(lsr::etaSquared(modelo))
print(sumario <- summary(modelo))
print(EMM <- emmeans::emmeans(modelo, "Grupo"))
print(grf <- plot(EMM,  colors = "black", 
                  main="Estimated Marginal Means",
                  xlab="Escore controlado por Motivacao",
                  ylab="Grupo"))
mc <- multcomp::glht(modelo, linfct = mcp(Grupo = "Tukey"))
print(mcs <- summary(mc, test=adjusted("bonferroni")))
multcomp::cld(mcs, level=alfa, decreasing=TRUE)
plot(mc,las=3)


