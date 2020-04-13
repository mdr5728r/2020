# Gestantes_matrizcorrelacoes.R

library(readxl)
library(GGally)

Gestantes <- read_excel("Gestantes.xlsx")
mc <- data.frame(Gestantes$IDADE, 
                 Gestantes$HT, 
                 Gestantes$HB, 
                 Gestantes$HEM, 
                 Gestantes$LEUC, 
                 Gestantes$FOLICO, 
                 Gestantes$B12)
names(mc) <- c("Idade","HT","HB","HEM","LEUC","FOLICO","B12")
print(head(mc))
cat("\n...\n")
print(tail(mc, addrownums = FALSE, n=2L))
cat("\nMatriz de correlacoes:\n")
print(cor(mc)) # matriz de correlacoes
# grafico da matriz
print(GGally::ggcorr(mc,
       nbreaks = 6,
       label = TRUE,
       label_size = 4,
       color = "#888888"))

