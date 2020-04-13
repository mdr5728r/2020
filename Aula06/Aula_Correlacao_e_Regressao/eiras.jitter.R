eiras.jitter <- function (valores, jitter=NA)
{
  if (is.na(jitter))
  {
    jitter <- (max(valores, na.rm=TRUE) - min(valores, na.rm=TRUE)) / 20
  }
  if (jitter > 0)
  {
    valores <- valores + runif(length(valores),-jitter,jitter)
  }
  
  return(valores)
}