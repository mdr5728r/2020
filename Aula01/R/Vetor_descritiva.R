# A partir de um vetor de dados
# computa medidas de localizacao e dispersao
# gera histograma, dorplot, boxplot e density plot
# cria arquivo .out com a saída numerica
# cria arquivos .png com os graficos

# Vetor de dados
dados <- c(117, 119, 122, 150, 181, 151, 184, 152, 188, 128, 134, 136, 136, 137, 138,
           141, 141, 142, 143, 143, 145, 145, 146, 148, 153, 154, 156, 157, 159, 162, 
           163, 165, 165, 166, 168, 170, 176, 176, 178, 192, 193, 193, 195, 196, 201, 
           202, 203, 207, 221, 226, 232, 250, 252)

# Nome do arquivo de saida (sem a extensao)
filebase <-"Resultado_colesterol" 

# Titulo para o grafico e eixo principal
grf_titulo_main <- "Distribuição das medidas de colesterol"
grf_titulo_axis <- "Colesterol sérico (mg/dl)"

# Tamanho do grafixo (em pixels)
largura = 700;
altura = 500;

fileout <- paste(filebase,"_output.txt",sep="")
filehist <- paste(filebase,"_histograma.png",sep="")
filedot  <- paste(filebase,"_dotplot.png",sep="")
filebox  <- paste(filebase,"_boxplot.png",sep="")
filedens <- paste(filebase,"_densityplot.png",sep="")

# abre o arquivo de saida
sink (fileout)

# Titulo
cat ("\n**************************")
cat ("\n* Estatística Descritiva *")
cat ("\n**************************\n")

# media e desvio padrao
media <- mean(dados, na.rm=TRUE)
dp <- var(dados, na.rm=TRUE)**0.5
# mediana e quartis
mediana <- median(dados, na.rm=TRUE)
quartil <- quantile(dados, probs=seq(0,1,0.25), na.rm=TRUE)
amplitude = quartil[5]-quartil[1]
iq = quartil[4]-quartil[2]
# moda, baseada na distribuicao de probabilidades
# assumindo que moda é o ponto mais alto da curva de densidades
dados_densidade <- density(dados, na.rm=TRUE)
moda <- dados_densidade$x[i.mode <- which.max(dados_densidade$y)]

# grava os resultados

# localizacao
cat ("\n\nMedidas de localizacao:")
cat ("\n media =", media)
cat ("\n mediana =", mediana)
cat ("\n moda =", moda)

# dispersao
cat ("\n\nMedidas de dispersao:")
cat ("\n d.p. =", dp)
cat ("\n quartis = \n")
print (quartil)
cat ("\n intervalo inter-quartil =", iq)
cat ("\n amplitude =", amplitude)
# registra o que salvou
cat ("\n\nResultado armazenado em",fileout)
cat ("\n - histograma em  ",filehist)
cat ("\n - dotplot em     ",filedot)
cat ("\n - boxplot em     ",filebox)
cat ("\n - density plot em",filedens)

# ative para possiveis problemas ocorridos
# cat ("\n\n")
# warnings()

# fecha o arquivo
sink()

# graficos

# histograma
png(filehist, width = largura, height = altura)
histograma <- hist (dados, main=grf_titulo_main,xlab=grf_titulo_axis,ylab="Frequência")
dev.off()

# stacked dotplot
png(filedot, width = largura, height = altura)
dotplot <- stripchart (dados, ylim = c(0,3),method="stack",  offset=0.5, at=0.15, pch=19, main=grf_titulo_main,xlab=grf_titulo_axis,ylab="Contagem")
dev.off()

# boxplot
png(filebox, width = largura, height = altura)
boxplot (dados, main=grf_titulo_main,xlab="",ylab=grf_titulo_axis)
dev.off()

# density plot
png(filedens, width = largura, height = altura)
plot (dados_densidade, main=grf_titulo_main,xlab=grf_titulo_axis,ylab="Densidade")
dev.off()

# ecoa onde achar o resultado quando executado com source() em terminal R
cat ("\nResultado armazenado em",fileout,"\n")
