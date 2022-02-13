library(dbplyr)

Example<-iris$Sepal.Length

par(mfrow = c(1, 2)) # arranges plots
qqnorm(Example)
hist(Example)

# cube root function
cube_root <- function(y) {
  sign(y) * abs(y)^(1/3)
}

different_transforms<- function(x) {
  a<-log(x)
  b<-log10(x)
  c<-exp(x)
  d<-abs(x)
  e<-sin(x)
  f<-cube_root(x)
  par(mfrow = c(2, 3))
  hist(a, main="log")
  hist(b, main="log10")
  hist(c, main="exp")
  hist(d, main="abs")
  hist(e, main="sine")
  hist(f, main="cube root")
}


# transform data and view
different_transforms(Example)
