# Teste t de Welch bilateral
# Referencial teorico da distribuicao t central e nao-central
nA <- 16; nB <- 25
mediaA <- 4.8; mediaB <- 5.6
dpA <- 1.8 ; dpB <- 1.2
alfa <- 0.05
dif <- mediaB - mediaA
dfA <- nA - 1; dfB <- nB - 1
ep2A <- dpA^2/nA; ep2B <- dpB^2/nB
df <- ((ep2A+ep2B)^2)/((ep2A^2)/dfA+(ep2B^2)/dfB)
t <- dif/sqrt(ep2A+ep2B)
p <- 2*pt(-abs(t),df)
eta2 <- t^2/(t^2 + df)
ncp <- t # ncp = estatistica de teste t
# media e dp das dist. t central e nao-central
mediaH0 <- 0
dpH0 <- sqrt(df/(df-2))
beta <- sqrt(df/2)*gamma((df-1)/2)/gamma(df/2)
mediaH1 <- ncp*beta
dpH1 <- sqrt(((df*(1+ncp^2))/(df-2))-mediaH1^2)
# distribuicao t sob H0 (central: ncp = 0)
tH0 <- rt(1e6, df)
dtH0 <- density(tH0)
# distribuicao t sob H1 (nao-central: ncp = t)
tH1 <- rt(1e6, df, ncp)
dtH1 <- density(tH1)
# quantis criticos de (1-alfa/2)% e poder
q <- c(qt(alfa/2,df), qt(alfa/2,df,lower.tail=FALSE))
# Grafico
x_min = max(c( min(c(dtH0$x,dtH1$x)), if(t<0){-2.5*abs(t)}else{-1.5*abs(q)} ))
x_max = min(c( max(c(dtH0$x,dtH1$x)), if(t>0){ 2.5*abs(t)}else{ 1.5*abs(q)} ))
# formato de p
p_txt <- paste("p = ",round(p,4),sep="")
if(p<1e-4){p_txt <- paste("p = ",format(p, format = "e", digits = 2),sep="")}
if(p==1){p_txt <- "p > 0.9999"}
plot(dtH0,type="l",
     main=paste("teste t de Welch bilateral\n",
                "alfa = ",alfa,", ",p_txt,", poder = ",round(poder,4),
                sep=""),
     sub=paste("t = ",round(t,2),", df = ",round(df,2),sep=""),
     xlab=NA,ylab="Densidade",
     xlim = c(x_min,x_max),
     lwd=1, lty=1, col="#000000"
)
lines(dtH1,lwd=3, lty=1, col="#000000")
abline(v=q[1], lwd=1, lty = 3, col="#000000")
abline(v=q[2], lwd=1, lty = 3, col="#000000")
abline(v=-abs(t), lwd=2, lty = 2, col="#000000")
abline(v=abs(t), lwd=2, lty = 2, col="#000000")
# area de alfa
# esquerda
alfa_x <- c(dtH0$x[dtH0$x<=q[1]]) 
alfa_y <- c(dtH0$y[dtH0$x<=q[1]])
lines(alfa_x, alfa_y, col="#444444", lwd=1)
alfa_x <- c(min(alfa_x),alfa_x,max(alfa_x))  
alfa_y <- c(0,alfa_y,0)
polygon(alfa_x, alfa_y, col="#44444488", border="#444444")
# direita
alfa_x <- c(dtH0$x[dtH0$x>=q[2]]) 
alfa_y <- c(dtH0$y[dtH0$x>=q[2]])
lines(alfa_x, alfa_y, col="#444444", lwd=1)
alfa_x <- c(min(alfa_x),alfa_x,max(alfa_x))  
alfa_y <- c(0,alfa_y,0)
polygon(alfa_x, alfa_y, col="#44444488", border="#444444")

# area beta (1-poder)
beta_x <- c(dtH1$x[dtH1$x>=q[1]&dtH1$x<=q[2]]) 
beta_y <- c(dtH1$y[dtH1$x>=q[1]&dtH1$x<=q[2]])
lines(beta_x, beta_y, col="#aaaaaa", lwd=3)
beta_x <- c(min(beta_x),beta_x,max(beta_x))  
beta_y <- c(0,beta_y,0)
polygon(beta_x, beta_y, col="#aaaaaa88", border="#aaaaaa")

# area de p
if (p>alfa) {p_cor="#333333"} else {p_cor="#ffffff"}
# esquerda
p_x <- c(dtH0$x[dtH0$x<=-abs(t)]) 
p_x <- c(min(p_x),p_x,max(p_x))  
p_y <- c(dtH0$y[dtH0$x<=-abs(t)])
p_y <- c(0,p_y,0)
hachura_x <- c()
hachura_y <- c()
for (a in seq(from=1, to=length(p_x), by=3))
{
  hachura_x <- c(hachura_x, p_x[a])
  hachura_y <- c(hachura_y, p_y[a])
}
if (max(hachura_y)<0.005) 
{
  hachura_y <- rep( 0.005,length(hachura_y)); p_cor="#000000";
}
lines(hachura_x,hachura_y,type="h",lwd=1,lty=1,col=p_cor)
# direita
p_x <- c(dtH0$x[dtH0$x>=abs(t)]) 
p_x <- c(min(p_x),p_x,max(p_x))  
p_y <- c(dtH0$y[dtH0$x>=abs(t)])
p_y <- c(0,p_y,0)
hachura_x <- c()
hachura_y <- c()
for (a in seq(from=1, to=length(p_x), by=3))
{
  hachura_x <- c(hachura_x, p_x[a])
  hachura_y <- c(hachura_y, p_y[a])
}
if (max(hachura_y)<0.005) 
{
  hachura_y <- rep( 0.005,length(hachura_y)); p_cor="#000000";
}
lines(hachura_x,hachura_y,type="h",lwd=1,lty=1,col=p_cor)
# legenda
if (abs(x_min)<abs(x_max)) {posleg <- "topright"} else {posleg <- "topleft"}
legend(posleg, 
       c(
         "dist. t (H0)", "dist. t (H1)", "t crítico", "t", "alfa", "beta"
       ), 
       col=c("#000000","#000000","#000000","#000000","#44444488","#aaaaaa88"),
       lwd=c(1,3,1,2,9,9), 
       lty=c(1,1,3,2,1,1), 
       box.lwd=0, bg="transparent")  

