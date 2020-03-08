library(readxl)
Dados <- read_excel(file.path("dados","Adm2008_v2.xlsx"))
N <- min(sum(!is.na(Dados$Estatura[Dados$Genero=="Feminino"])), 
         Dados$MCT[Dados$Genero=="Feminino"])
plot(Dados$Estatura[Dados$Genero=="Feminino"], 
     Dados$MCT[Dados$Genero=="Feminino"],
     main=paste("Estudantes femininas de Administração Noturno 
        FEA-USP 2008", "\nN =",N), 
     xlab="Estatura (cm)", 
     ylab=" Massa Corporal Total (kg)")