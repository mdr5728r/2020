library("psych")
# Medias aritmetica de estatura masculina
estatm <- c(176, 183, 173, 191, 177.7, 197, 168.9, 181, NA, 169)
cat("estatm:",estatm,"\n")
media <- mean(estatm, na.rm=TRUE)
cat("media aritmetica:",media,"\n")
# Mediana e quartis
mediana <- median(estatm, na.rm=TRUE)
quantil <- quantile(estatm, na.rm=TRUE)
cat("mediana:",mediana,"\n")
cat("quantis:\n")
print(quantil)
# Media aritmetica robusta
cat("\nMedia robusta\n")
# Remove NA, ordena, remove 10% de cada extremidade e calcula a media aritmetica
media <- mean(estatm, na.rm=TRUE, trim=0.10) 
estatm.ord <- sort(estatm) # remove NA e ordena
cat("estatm.ord:",estatm.ord,"\n")
media <- mean(estatm.ord[2:9], na.rm=TRUE)
cat("media aritmetica robusta:",media,"\n")
# Medias geometrica e harmonica
cat("\nUsando package psych:\n")
mgeom <- psych::geometric.mean(estatm, na.rm=TRUE)
mharm <- psych::harmonic.mean(estatm, na.rm=TRUE, zero=TRUE)
cat("media geometrica:",mgeom,"\n")
cat("media harmonica:",mharm,"\n")