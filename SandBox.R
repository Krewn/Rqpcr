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

# m9(q)
# D(expression(x^3-6*x^2), 'x') 
# which.max(diff(diff(q[[10]])))

# xyr <- D(expression(14252/(1+exp((-1/274.5315)*(x-893)))), 'x')
# 
# D2 <- D(xyr, 'x')
# 
# f = function(x) eval(D2)
# tr <- uniroot(f,c(0,1000))
# 
# print(tr$root)

# m3Fluo <- list()
# m4Fluo <- list()
m5Fluo <- list()
m6Fluo <- list()
#m7Fluo <- list()
m8Fluo <- list()

m9Fluo <- list()
m9Fluo <- m9(q)


# m3Fluo <- m3(q)
# m4Fluo <- m4(q)
#m7Fluo <- m7(q)
m5Fluo <- m5(q)
m6Fluo <- m6(q)
m8Fluo <- m8(q)


plot(log(unlist(m7Fluo)), log(unlist(m5Fluo)), xlab="Cy0 Method", ylab="LinRegPCR")
plot(log(unlist(m7Fluo)), log(unlist(m6Fluo)), xlab="Cy0 Method", ylab="LRE-Emax")
plot(log(unlist(m7Fluo)), log(unlist(m8Fluo)), xlab="Cy0 Method", ylab="5PSM")
plot(log(unlist(m5Fluo)), log(unlist(m8Fluo)), xlab="LinRegPCR", ylab="5PSM")
