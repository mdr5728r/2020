library(readxl)
library(car)
Dados <- read_excel(file.path("dados","Adm2008_v2.xlsx"))
densityPlot(Estatura~factor(Genero), data=Dados, bw=bw.SJ, 
            adjust=1, kernel=dnorm, method="adaptive")