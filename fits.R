#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
#############################################

polyFit <- function(xs,ys,degree) { #polynomial fitting a data set degree 3
  fit3 <- lm(ys~poly(xs,degree,raw=TRUE))
  xx <- seq(0,160, length=50)
  plot(xs,ys,pch='@')
  lines(xx, predict(fit3, data.frame(xs=xx)), col="green")
}

logFit <- function(xs,ys) { #graph the data set with log(x), y
  logEstimate = lm(ys ~ log(xs))
  plot(xs,ys,pch='@')
  lines(xs,predict(logEstimate),col='green')
}

eFit <- function(xs,ys) {
  expEstimate = lm(log(ys) ~ xs)
  plot(xs, ys,pch='@')
  lines(xs, exp(predict(expEstimate)),col='green')
}


