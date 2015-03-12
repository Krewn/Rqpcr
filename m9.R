
m9 <- function(data1) {
     f1 <- p4Logistic(data1[[10]])

     xt <- seq(1, length(unlist(f1)))
     plot(xt, f1)
}