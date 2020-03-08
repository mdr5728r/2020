# Dataframe_Cria_e_Conta.R
# a partir de um vetor com variaveis nominais, cria um dataframe 
# e computa contagem, freq. relativa e freq. acumulada
Metodos <- c("Tabelinha", "Tabelinha", "Preservativo", "Pílula", "Pílula", "Tabelinha",  
             "Tabelinha", "Preservativo", "Pílula", "Preservativo", "Preservativo",  
             "Preservativo", "Outro", "Pílula", "Preservativo", "Tabelinha", "Tabelinha",  
             "Outro", "Pílula", "Outro", "Preservativo", "Pílula", "Tabelinha", "Tabelinha",  
             "Tabelinha", "Pílula", "Tabelinha", "Preservativo", "Preservativo",  
             "Tabelinha", "Outro", "Tabelinha", "Tabelinha", "Preservativo", "Pílula",  
             "Pílula", "Preservativo", "Preservativo", "Preservativo", "Outro",  "Tabelinha", 
             "Tabelinha", "Pílula", "Tabelinha", "Pílula", "Preservativo",  "Preservativo", 
             "Tabelinha", "Preservativo", "Pílula", "Tabelinha",  "Tabelinha", "Preservativo", 
             "Preservativo", "Pílula", "Tabelinha",  "Tabelinha", "Pílula", "Preservativo", 
             "Tabelinha")
contagem <- table(Metodos)
freqrel <- as.numeric(contagem/sum(contagem)*100)
dt_descricao <- data.frame(contagem, freqrel)
dt_descricao$freqacm <- 0
names(dt_descricao) <- c("Metodo", "Contagem", "Freq.rel", "Freq.acm")
dt_descricao <- dt_descricao[order(dt_descricao$Contagem,
                                   decreasing = TRUE),]
acm <- 0
for (i in 1:nrow(dt_descricao))
{
  acm <- acm+dt_descricao$Freq.rel[i]
  dt_descricao$Freq.acm[i] <- acm  
}
print (dt_descricao)