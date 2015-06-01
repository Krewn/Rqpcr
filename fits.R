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

p4Logistic <- function(data1) {
     Fx <- list()
     y0 <- data1[[1]]
     a <- (max(data1) - y0)
     Fprime <- derivative(data1)
     x0 <- 0
     b <- 0
     cycle <- 0
     
     for(k in 1:length(Fprime)) {
          if(Fprime[[k]] > x0) {
               x0 = Fprime[[k]]
               cycle = k
          }
     }
     
     b <- Fprime[[cycle+1]] - Fprime[[cycle]]
     
     for(k in 1:length(data1)) {
          Fx[[k]] <- (y0 + a/(1 + (k/x0)^b))
     }
     
     return(Fx)
}

derivative <- function(data1) {
     Fprime <- list()
     
     for(k in 1:(length(data1)-1)) {
          Fprime[[k]] <- data1[[k+1]] - data1[[k]]
     }
     
     return(Fprime)
}


