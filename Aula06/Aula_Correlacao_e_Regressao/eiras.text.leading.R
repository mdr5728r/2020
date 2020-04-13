# returns color in format #rrggbbaa

eiras.text.leading <- function (text, lentxt, lead.chr=" ")
{
  while(nchar(text) < lentxt)
  {
    text <- paste(lead.chr,text,sep="")  
  }
  return(text)
}
