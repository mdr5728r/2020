# plot of confidence intervals
# receives data.frame with 4 or 5 columns
#   labels, mean, lower, upper, {p.values}
#   usecolor = "y|n"

source("friendlycolor.R")

eiras_plotIC <- function(df.data, main=NA, xlab=NA, usecolor="n")
{
  # add column 5 if p.values do not exist
  if(ncol(df.data)==4)
  {
    df.data$p <- NA
  }
  
  minx <- min(df.data[,2:4],na.rm=TRUE) 
  maxx <- max(df.data[,2:4],na.rm=TRUE)
  range <- abs((maxx-minx)*0.15)
  minx <- minx - range
  maxx <- maxx + range
  plot(NA, axes=FALSE,
       main=main,
       xlab = xlab, ylab = "", 
       xlim = c(minx,maxx),
       ylim = c(0,nrow(df.data)+1)
  )
  axis(side=2, at=1:nrow(df.data), labels=df.data[1:nrow(df.data),1], las=1)
  axis(side=1)
  for (r in 1:nrow(df.data))
  {
    color.line <- "black" 
    color.pto <- "black" 
    if (df.data[r,3]<=0 & 0<=df.data[r,4] )
    {
      # non significant
      if(usecolor=="y") {color.line <- friendlycolor(8)} else
      {color.line <- friendlycolor(39)}
    } else
    {
      # significant
      if(usecolor=="y") {color.line <- friendlycolor(20)} else
      {color.line <- friendlycolor(36); color.pto <- friendlycolor(43)}
    }
    lines(
      c(df.data[r,3],df.data[r,4]),
      rep(r,2),
      lwd=8, col=color.line
      )
    points(df.data[r,2], r, 
           pch=21, bg="black", col=color.pto, cex=1.5
           )
    if(!is.na(df.data[r,5]))
    {
      if (df.data[r,5] >= 1e-4)
      {
        p.txt <- sprintf("p = %.4f",df.data[r,5])
      } else
      {
        p.txt <- sprintf("p = %.2e",df.data[r,5])
      }
      if (round(df.data[r,5],4) == 1)
      {
        p.txt <- "p > 0.9999"
      } 
      text(df.data[r,2], r-0.3, p.txt)
    }
  }
  abline(v=0,lty=2)
}

# t.ic <- mcs.tukey$test$qfunction(0.95)
# df_teste <- data.frame(
#   names (mcs.tukey$test$coefficient),
#   as.vector(mcs.tukey$test$coefficients),
#   as.vector(mcs.tukey$test$coefficients)-t.ic*as.vector(mcs.tukey$test$sigma),
#   as.vector(mcs.tukey$test$coefficients)+t.ic*as.vector(mcs.tukey$test$sigma),
#   as.vector(mcs.tukey$test$pvalues)
# )
# eiras_plotIC(df_teste, 
#              main="95% family-wise confidence level", 
#              xlab="Difference",
#              usecolor="y"
#              )
