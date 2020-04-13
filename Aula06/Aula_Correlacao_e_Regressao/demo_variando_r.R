source ("BiNormal.R")

for (r in seq(-1,1,0.2))
{
  dados <- binormal(correl=r, n=1000, pathname="image",
                    plot="y", axes=FALSE)
}
r <- 1
for (s in seq(0.0001,0.8001,0.25))
{
  dados <- binormal(correl=r, n=1000, pathname="image",
                    scale2 = s,
                    plot="y", axes=FALSE)
}
r <- -1
for (s in seq(0.0001,0.8001,0.25))
{
  dados <- binormal(correl=r, n=1000, pathname="image",
                    scale2 = s,
                    plot="y", axes=FALSE)
}
