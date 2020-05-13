# https://rcompanion.org/handbook/F_08.html 
library(readxl)
library(rcompanion)
library(FSA)
library(ggplot2)
library(lattice)
Dados <- readxl::read_excel("Terapia para enxaqueca.xlsx")
sink("TesteKruskal-Wallis_enxaqueca.txt")
pdf("TesteKruskal-Wallis_enxaqueca,pdf")
xtabs( ~ Grupo + Sintoma2,
       data = Dados)
lattice::histogram(~ Sintoma2 | Grupo,
          data=Dados,
          layout=c(1,3)      #  columns and rows of individual plots
)
with(Dados, kruskal.test(Sintoma2,Grupo))
with(Dados, rcompanion::epsilonSquared(Sintoma2,Grupo))
print(dt <- FSA::dunnTest(Sintoma2~Grupo,data=Dados,method="bh"))
pt <- dt$res
rcompanion::cldList(P.adj ~ Comparison, data = pt,threshold = 0.05)     
Sum <- rcompanion::groupwiseMedian(Sintoma2~Grupo,
                      data       = Dados,
                      conf       = 0.95,
                      boot = TRUE,
                      R          = 1e4,
                      percentile = TRUE,
                      bca        = FALSE,
                      digits     = 3)
X <- 1:3
Y <- Sum$Percentile.upper + 0.2
Label <- c("a", "b", "a")
ggplot2::ggplot(Sum,                ### The data frame to use.
       aes(x = Grupo,
           y = Median)) +
  geom_errorbar(aes(ymin = Percentile.lower,
                    ymax = Percentile.upper),
                width = 0.05,
                size  = 0.5) +
  geom_point(shape = 15,
             size  = 4) +
  theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) +
  ylab("Median score") +
  annotate("text",
           x = X,
           y = Y,
           label = Label)
dev.off()
sink()

