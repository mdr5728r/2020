library(readxl)
library(gplots)
library(car)
library(lsr)
library(emmeans)
library(multcomp)
library(ggplot2)
alfa <- 0.05
Dados <- readxl::read_excel("Tomato.xlsx")
Dados$Fertilizante <- factor(Dados$Fertilizante,
                             levels=unique(Dados$Fertilizante))
head(Dados)
with(Dados, gplots::plotmeans(AlturaFinal ~ Fertilizante, 
            error.bars="conf.int", level=1-alfa, connect=FALSE,
            xlab="Fertilizante", ylab="Altura Final (mm)", main="IC95%",
            barcol="black"))
with(Dados, car::scatterplot(AlturaInicial, AlturaFinal, 
                             groups = Fertilizante, 
            regLine=TRUE, smooth=FALSE, ellipse=FALSE, col="black", 
            xlab = "Altura Inicial (mm)", ylab = "Altura Final (mm)"))
print(ggplot(Dados, aes(y = AlturaFinal, x = AlturaInicial, 
                    group = Fertilizante, linetype = Fertilizante)) + 
                    geom_point() + geom_smooth(method = "lm"))
# VD: AlturaFinal VI: Fertilizante
modelo <- lm(AlturaFinal ~ Fertilizante, data=Dados)
print(Anova <- car::Anova(modelo, type=3, white.adjust=TRUE))
print(lsr::etaSquared(modelo))
print(sumario <- summary(modelo))
print(EMM <- emmeans::emmeans(modelo, "Fertilizante"))
print(grf <- plot(EMM,  colors = "black", 
                  main="Estimated Marginal Means",
                  xlab="Altura Final (mm)",
                  ylab="Fertilizante"))
mc <- multcomp::glht(modelo, linfct = mcp(Fertilizante = "Tukey"))
print(mcs <- summary(mc, test=adjusted("bonferroni")))
multcomp::cld(mcs, level=alfa, decreasing=TRUE)
plot(mc,las=3)
# VD: AlturaFinal - AlturaInicial VI: Fertilizante
Diferenca <- Dados$AlturaFinal - Dados$AlturaInicial
modelo <- lm(Diferenca ~ Fertilizante, data=Dados)
print(Anova <- car::Anova(modelo, type=3, white.adjust=TRUE))
print(lsr::etaSquared(modelo))
print(sumario <- summary(modelo))
print(EMM <- emmeans::emmeans(modelo, "Fertilizante"))
print(grf <- plot(EMM,  colors = "black", 
                  main="Estimated Marginal Means",
                  xlab="AlturaFinal - AlturaInicial (mm)",
                  ylab="Fertilizante"))
mc <- multcomp::glht(modelo, linfct = mcp(Fertilizante = "Tukey"))
print(mcs <- summary(mc, test=adjusted("bonferroni")))
multcomp::cld(mcs, level=alfa, decreasing=TRUE)
plot(mc,las=3)
# ANCOVA
## ANCOVA: Teste de dissociacao entre fator e covariavel
cat("\nANCOVA: Teste de dissociacao entre fator e covariavel\n")
modelo <- lm(AlturaInicial ~ Fertilizante, data=Dados)
print(Anova <- car::Anova(modelo, type=3, white.adjust=TRUE))
## ANCOVA: Teste de igualdade das inclinacoes das retas de regressao
cat("\nANCOVA: Teste de igualdade das inclinacoes das retas de regressao\n")
modelo <- lm(AlturaFinal ~ Fertilizante + AlturaInicial + Fertilizante*AlturaInicial, 
             data=Dados)
print(Anova <- car::Anova(modelo, type=3))
## ANCOVA: Teste do efeito do fator fixo: 
##          Se as declividades sao iguais, testar se os interceptos sao iguais.
cat("\nANCOVA: Teste do efeito do fator fixo:\n")
cat("\tSe as declividades sao iguais, testar se os interceptos sao iguais.\n")
modelo <- lm(AlturaFinal ~ Fertilizante + AlturaInicial, 
             data=Dados)
print(Anova <- car::Anova(modelo, type=3, white.adjust=TRUE))
print(lsr::etaSquared(modelo))
print(sumario <- summary(modelo))
print(EMM <- emmeans::emmeans(modelo, "Fertilizante"))
print(grf <- plot(EMM,  colors = "black", 
                  main="Estimated Marginal Means",
                  xlab="AlturaFinal controlada por AlturaInicial(mm)",
                  ylab="Fertilizante"))
mc <- multcomp::glht(modelo, linfct = mcp(Fertilizante = "Tukey"))
print(mcs <- summary(mc, test=adjusted("bonferroni")))
multcomp::cld(mcs, level=alfa, decreasing=TRUE)
plot(mc,las=3)


