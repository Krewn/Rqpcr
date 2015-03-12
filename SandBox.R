#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
source("./qpcRsources.R")                   #
#############################################

gd <- getData()
CT <<- list()

n <- gdToDf(gd)  

# plot(pcrfit(n[[2]], 1,2,l4))
# title(main=names(n)[2])
# plot(pcrfit(n[[3]], 1,2,l4))
# title(main=names(n)[3])
# plot(pcrfit(n[[4]], 1,2,l4))
# 
# title(main=names(n)[4])
# plot(pcrfit(n[[5]], 1,2,l4))
# title(main=names(n)[5])

q <- n[[4]][2:length(n[[4]])] - n[[2]][2:length(n[[2]])]
q[[1]] <- n[[4]][[1]]

# m1c(q)
# t1 <- c(1, 4, 9, 16, 25, 36)
# t2 <- derivative(t1)
# xt <- seq(1, length(unlist(t2)))
# plot(t1)
# lines(xt, t2)


# m9(q)

m5Fluo <- list()
m6Fluo <- list()
m7Fluo <- list()

m7Fluo <- m7(q)
m5Fluo <- m5(q)
m6Fluo <- m6(q, 30)

m7Fluo[[911]] <- NULL

plot(log(unlist(m7Fluo)), log(unlist(m5Fluo)), xlab="Cy0 Method", ylab="LinRegPCR")
plot(log(unlist(m7Fluo)), log(unlist(m6Fluo)), xlab="Cy0 Method", ylab="LRE-Emax")
