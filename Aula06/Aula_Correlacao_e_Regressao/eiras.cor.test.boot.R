# eiras.cor.test.boot.R
# method = c("pearson", "spearman") para correlacao
# method = c("lm", "lm_robust") para regressao 

library(dplyr)
library(estimatr)

source("eiras.friendlycolor.R")
source("eiras.jitter.R")
source("eiras.col2rgbstring.R")

eiras.cor.test.boot <- function (x, y, alpha=NA, B=NA,
                                 z.score=FALSE,
                                 method = "pearson",
                                 jitter=NA,
                                 main="",
                                 xlab="", ylab="",
                                 xlim=NA, ylim=NA,
                                 col="black", bg="black", pch=21,
                                 suppress.graph=FALSE,
                                 suppress.text=FALSE)
{
  if (z.score==TRUE)
  {
    mean.x <- mean(x, na.rm=TRUE)
    sd.x <- sd(x, na.rm=TRUE)
    x <- (x-mean.x)/sd.x
    mean.y <- mean(y, na.rm=TRUE)
    sd.y <- sd(y, na.rm=TRUE)
    y <- (y-mean.y)/sd.y
  }
  df_boot <- data.frame(x, y)
  if (sum(is.na(xlim))>0)
  {
    min.x <- min(x, na.rm = TRUE)
    max.x <- max(x, na.rm = TRUE)
  } else
  {
    xlim <- unlist(xlim)
    min.x <- xlim[1]
    max.x <- xlim[2]
  }
  if (sum(is.na(ylim))>0)
  {
    min.y <- min(y, na.rm = TRUE)
    max.y <- max(y, na.rm = TRUE)
  } else
  {
    ylim <- unlist(ylim)
    min.y <- ylim[1]
    max.y <- ylim[2]
  }
  
  # iterations
  if(is.na(B))
  {
    B <- 1e4
  }
  # significance level
  if(is.na(alpha))
  {
    alpha <- 0.05
  }
  # colors
  col <- eiras.col2rgbstring(col)
  bg <- eiras.col2rgbstring(bg)
  # transparency
  tp <- "ff"
  if (B > 0)
  {
    tp <- 5000/B
    if (tp < 3) {tp <- 3}
    if (tp > 25) {tp <- 25}
    tp <- eiras.text.leading(as.character(as.hexmode(as.numeric(tp))),2,"0")
  }

  # correlacoes
  if (
    substr(method,1,1)=="p" |
    substr(method,1,1)=="s"
  )
  {
    # original cor.test
    original.correlacao <- with(df_boot, cor.test(x,y, conf.level = 1-alpha, method = method))
    # p.value em forma textual (troca se tiver B>0)
    p.txt <- sprintf("%.4f",original.correlacao$p.value)
    if (original.correlacao$p.value < 1e-4)
    {
      p.txt <- sprintf("%.2e",original.correlacao$p.value)
    }
    # graph title
    title <- ""
    if (nchar(main)>0) {title <- paste(main,"\n",sep="")}
    title <- paste(title,"r = ",round(as.numeric(original.correlacao$estimate),3),
                   ", IC",round((1-alpha)*100,0),"[",round(as.numeric(original.correlacao$conf.int[1]),3),
                   ", ",round(as.numeric(original.correlacao$conf.int[2]),3),"]",
                   "\np = ",p.txt,
                   sep="")
    
    # bootstrapping
    if (B > 0)
    {
      estimate <- c()
      pvalue <- c()
      tvalue <- c()
      intercepto <- c()
      inclinacao <- c()
      # x do IC
      columns.ic <- 20
      x_ic <- seq(min(x,na.rm=TRUE), 
                  max(x,na.rm=TRUE), 
                  length.out=columns.ic)
      # y do IC
      df_y_ic <- data.frame(matrix(nrow=0, ncol=columns.ic))
      names(df_y_ic) <- c(x_ic)
      for (b in 1:B)
      {
        df_sample <- dplyr::sample_n(df_boot, size=nrow(df_boot), replace=TRUE)
        names(df_sample) <- c("x","y")
        correlacao <- with(df_sample, cor.test(x, y, conf.level = 1-alpha, method = method))
        estimate <- c(estimate, as.numeric(correlacao$estimate))
        pvalue <- c(pvalue, as.numeric(correlacao$p.value))
        tvalue <- c(tvalue, as.numeric(correlacao$statistic))
        if (substr(method,1,1)=="p")
        {
          modelo <- with(df_sample, lm(y ~ x))
          intercepto <- c(intercepto, as.numeric(modelo$coefficients[1]))
          inclinacao <- c(inclinacao, as.numeric(modelo$coefficients[2]))
          # acumula para IC da banda de confianca
          y_ic <- as.numeric(modelo$coefficients[1]) +
            as.numeric(modelo$coefficients[2])*x_ic
          # y_ic <- rnorm(length(x_ic),40,5)
          df_tmp <- data.frame(matrix(y_ic, nrow=1, ncol=columns.ic))
          names(df_tmp) <- c(x_ic)
          df_y_ic <- rbind(df_y_ic,df_tmp)
        }
      }
      r <- quantile(estimate, probs=c(alpha/2, 0.5, 1-alpha/2))
      p <- quantile(pvalue, probs=c(alpha/2, 0.5, 1-alpha/2))
      t <- quantile(tvalue, probs=c(alpha/2, 0.5, 1-alpha/2))
      df <- as.numeric(correlacao$parameter)
      # ic
      y_ic_lower <- c()
      y_ic_median <- c()
      y_ic_upper <- c()
      for (c in 1:ncol(df_y_ic))
      {
        q <- quantile(df_y_ic[,c], probs=c(alpha/2, 0.5, 1-alpha/2))
        y_ic_lower <- c(y_ic_lower, as.numeric(q[1]))
        y_ic_median <- c(y_ic_median, as.numeric(q[2]))
        y_ic_upper <- c(y_ic_upper, as.numeric(q[3]))
      }
      # p.value em forma textual
      p.txt <- sprintf("%.4f",p[2])
      if (p[2] < 1e-4)
      {
        p.txt <- sprintf("%.2e",p[2])
      }
      # graph title
      title <- ""
      if (nchar(main)>0) {title <- paste(main,"\n",sep="")}
      title <- paste(title,
                     "r = ",round(r[2],3),
                     ", IC",round((1-alpha)*100,0),"[",round(r[1],3),
                     ", ",round(r[3],3),"]",
                     "\np = ",p.txt,
                     sep="")
    }
  } # correlacoes
  
  # regressoes
  if (
    method=="lm" |
    method=="lm_robust"
    )
  {
    if (method=="lm")
    {
      rls <- lm(y ~ x)
    }
    if (method=="lm_robust")
    {
      rls <- estimatr::lm_robust(y ~x)
    }
    regressao <- summary(rls)

    b0 <- rls$coefficients[1]
    b1 <- rls$coefficients[2]

    min.x <- min(x,na.rm=TRUE)
    max.x <- max(x,na.rm=TRUE)
    min.y <- min(y)-sd(y)
    max.y <- max(y)+sd(y)
    
    reg_x <- seq(from=min.x, to=max.x, length.out = 100)
    reg_y <- b0+b1*reg_x
    centr_x <- mean(x, na.rm=TRUE)
    centr_y <- mean(y, na.rm=TRUE)
    sdx <- sd(x, na.rm=TRUE)
    sdy <- sd(y, na.rm=TRUE)

    if (method=="lm")
    {
      MSE = sum(rls$residuals^2)/rls$df.residual
      RSE = sqrt(MSE)
      n = length(y)
      x0 = seq(min.x, max.x, length = 100)
      SE = RSE*sqrt(1/n + ((x0 - centr_x)^2)/(sum((x-centr_x)^2)))
      # Banda de confianca de 95% de Working-Hotelling-Scheffe
      # Applied linear statistical model - Kutner et al - 5e - 2004, p. 63
      # Applied multivariate statistical analysis - Johnson & Wichern - 6e - 2007, p. 212
      # W2 = T2(p,n-1) = ((n-1)/(n-p))pF(p,n-p), p = 2 
      Band = sqrt(((n-1)/(n-2))*2*qf(1-alpha,2,n-2))*SE
      Yhat = b0 + b1*x0
      UB = Yhat + Band
      LB = Yhat - Band
    }
    
    if (method == "lm_robust")
    {
      F <- regressao$fstatistic[1]
      df <- regressao$fstatistic[2]
      dfErro <- regressao$df.residual
      s <- sd(y,na.rm=TRUE)
      n <- regressao$N
      if (regressao$r.squared == -Inf) {eta2 <- R2 <- 0} 
      if (regressao$r.squared != -Inf) {eta2 <- R2 <- regressao$r.squared}
      SEE <- s*sqrt(((n-1)/(n-2))*(1-eta2))
      Rmult <- sqrt(R2)
      if (b1<0) {Rmult <- -Rmult}
      if (regressao$adj.r.squared == -Inf) {R2aj <- 0} 
      if (regressao$adj.r.squared != -Inf) {R2aj <- regressao$adj.r.squared}
      lid <- min(x,na.rm=TRUE)
      lsd <- max(x,na.rm=TRUE)
      centrado_x <- x - mean(x,na.rm=TRUE)
      rlsr.centr <- estimatr::lm_robust(y ~ centrado_x)
      b0.centr <- rlsr.centr$coefficients[1]
    }

    # graph title
    p.txt <- sprintf("%.4f",regressao$coefficients[2,4])
    if (regressao$coefficients[2,4] < 1e-4)
    {
      p.txt <- sprintf("%.2e",regressao$coefficients[2,4])
    }
    title <- ""
    if (nchar(main)>0) {title <- paste(main,"\n",sep="")}
    title <- paste(title,
                   "y_medio = ",round(b0,4)," + ",round(b1,4),"x",
                   "\nR^2 = ",round(as.numeric(regressao$r.squared),3),
                   " (p = ",p.txt,")",
                   sep="")
    
  }
  # grafico
  plot(eiras.jitter(x,jitter), eiras.jitter(y,jitter),
       xlim=c(min.x,max.x), ylim=c(min.y,max.y),
       main=title,
       xlab=xlab, ylab=ylab,
       col=col, bg=bg, pch=pch
       )
  if (z.score==TRUE)
  {
    abline(v=0,lty=2)
    abline(h=0,lty=2)
  }
  
  if (
    method=="lm" |
    method=="lm_robust"
  )
  {
    # regressao line (somin.x for vamin.x segment)
    lines(reg_x,reg_y,lty=1,lwd=2)
    # centroid
    points(centr_x, centr_y, cex = 1, pch=19)
    # IC
    if (method=="lm")
    {
      lines(x0, UB, lty=2)
      lines(x0, LB, lty=2)
    }
  }
  if (substr(method,1,1)=="p")
  {
    if (B > 0)
    {
      x_tp <- c(min(x,na.rm=TRUE), max(x,na.rm=TRUE))
      col_tp <- paste(eiras.friendlycolor(24),tp,sep="")
      lwd <- 0.4
      for (b in 1:B)
      {
        y_tp <- intercepto[b] + inclinacao[b]*x_tp
        lines(x_tp, y_tp, col=col_tp, lwd=lwd)
      }
      # banda de IC
      lines(x_ic, y_ic_lower, lty=2)
      lines(x_ic, y_ic_median, lty=1)
      lines(x_ic, y_ic_upper, lty=2)
    } else
    {
      original.modelo <- with(df_boot, lm(y ~ x))

      col_tp <- bg
      x_tp <- c(min(df_boot$x,na.rm=TRUE), max(df_boot$x,na.rm=TRUE))
      y_tp <- as.numeric(original.modelo$coefficients[1]) +
        as.numeric(original.modelo$coefficients[2]) * x_tp
      lwd <- 2
      lines(x_tp, y_tp, col=col_tp, lwd=lwd)
    }
  }
  # saida textual
  if (suppress.text==FALSE)
  {
    # estatistica descritiva
    cat("\n",xlab,"\n")
    print(summary(x))
    cat("\n",ylab,"\n")
    print(summary(y))
    
    # correlacoes
    if (
      substr(method,1,1)=="p" |
      substr(method,1,1)=="s"
    )
    {
      # correlacao e regressao
      if (B > 0)
      {
        # bootstrapping
        cat ("\n\t")
        if (substr(method,1,1)=="p") {cat ("Pearson")}
        if (substr(method,1,1)=="s") {cat ("Spearman")}
        cat ("'s r (bootstrapping with ",B," iterations):\n",sep="")
        cat ("\nr:\t",r[2]," [",r[1],", ",r[3],"]", sep="")
        if (substr(method,1,1)=="p") 
        {
          cat ("\nt:\t",t[2],", df: ",df, sep="")
        }
        cat ("\np:\t",p[2], sep="")
        cat("\n")
      }
      # original cor.test
      print(original.correlacao)
      
    }
    # regressoes
    if (
      method=="lm" |
      method=="lm_robust"
    )
    {
      print (regressao)
      if (method=="lm_robust")
      {
        cat("\ndesvio-padrao de ",ylab," = ",s,"\nErro-padrao da estimativa de ",ylab," = ",
            SEE,"\n", sep="")
        cat("R multiplo = ",Rmult,"\nR^2 = coefic. de determinacao = ", R2,"\n", sep="")
        cat("Eta^2 de ",xlab," = ",eta2,"\n\n",sep="")
        cat(ylab,"_media = ",b0," + ",b1," * ",xlab," [",lid,",",lsd,"]\n\n",sep="")
        cat(ylab,"_media = ",b0.centr," + ",b1," * ",xlab,"_centrada [",
            lid - mean(x,na.rm=TRUE),",",
            lsd - mean(x,na.rm=TRUE),"]\n\n",sep="")
      }
      
    }
    
  } # suppress.text
}

