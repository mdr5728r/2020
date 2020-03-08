# Dataframe_Descritiva_Grafico.R
# importa planilha Excel
# mostra uma visao geral dos dados
# gera boxplot e density plot das variaveis numericas
# gera barplot e pie plot das variaveis em texto (nominais ou ordinais)

library(readxl)
Dados <- read_excel(file.path("dados","Adm2008_v2.xlsx"))

coltipo <- as.vector(sapply(Dados, typeof))
for (i in 1:length(Dados))
{
  # titulo 
  grf_titulo_main <- names(Dados)[i]
  
  # tipos numericos
  if (coltipo[i] == "double" || coltipo[i] == "integer" )
  {
    boxplot (Dados[i], xlab="",ylab=grf_titulo_main)
    dados_densidade <- density(Dados[[i]], na.rm = TRUE)
    plot (dados_densidade, main=NA, xlab=grf_titulo_main, ylab="Densidade")
  }
  # tipos textuais
  if (coltipo[i] == "character")
  {
    dt_col <- table(Dados[i])
    nomes <- names(dt_col)
    fatias <- as.numeric(dt_col)
    pie(fatias, labels=nomes, main=grf_titulo_main)
    barplot(fatias, names.arg = nomes, main=grf_titulo_main)
  }
}

# 
# # Titulo
# cat ("\n**************************")
# cat ("\n* Estatística Descritiva *")
# cat ("\n**************************\n")
# 
# # media e desvio padrao
# media <- mean(dados)
# dp <- var(dados)**0.5
# # mediana e quartis
# mediana <- median(dados)
# quartil <- quantile(dados, probs=seq(0,1,0.25))
# amplitude = quartil[5]-quartil[1]
# iq = quartil[4]-quartil[2]
# # moda, baseada na distribuicao de probabilidades
# # assumindo que moda é o ponto mais alto da curva de densidades
# dados_densidade <- density(dados)
# moda <- dados_densidade$x[i.mode <- which.max(dados_densidade$y)]
# 
# # grava os resultados
# # localizacao
# cat ("\n\nMedidas de localizacao:")
# cat ("\n media =", media)
# cat ("\n mediana =", mediana)
# cat ("\n moda =", moda)
# # dispersao
# cat ("\n\nMedidas de dispersao:")
# cat ("\n d.p. =", dp)
# cat ("\n quartis = \n")
# print (quartil)
# cat ("\n intervalo inter-quartil =", iq)
# cat ("\n amplitude =", amplitude)
# 
# # registra o que salvou
# cat ("\n\nResultado armazenado em",fileout)
# cat ("\n - histograma em  ",filehist)
# cat ("\n - boxplot em     ",filebox)
# cat ("\n - density plot em",filedens)
# 
# # ative para possiveis problemas ocorridos
# # cat ("\n\n")
# # warnings()
# 
# # fecha o arquivo
# sink()
# 
# # graficos
# 
# # histograma
# png(filehist, width = largura, height = altura)
# histograma <- hist (dados, main=grf_titulo_main,xlab=grf_titulo_axis,ylab="Frequência")
# dev.off()
# # boxplot
# png(filebox, width = largura, height = altura)
# boxplot (dados, main=grf_titulo_main,xlab="",ylab=grf_titulo_axis)
# dev.off()
# # density plot
# png(filedens, width = largura, height = altura)
# plot (dados_densidade, main=grf_titulo_main,xlab=grf_titulo_axis,ylab="Densidade")
# dev.off()
# 
# # ecoa onde achar o resultado quando executado com source() em terminal R
# cat ("\nResultado armazenado em",fileout,"\n")
