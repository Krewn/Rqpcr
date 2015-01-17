#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
source("./qpcRsources.R")                   #
#############################################

gd <- getData()
LOLprint(gd)
n <- gdToDf(gd)  
#for(k in n){print(c(k)}
#pcrfit(as.data.frame(n[[3]]), cyc=1, 1, start = NULL, offset= 0, weight =NULL ,verbose=TRUE)

plot(pcrfit(n[[2]], 1,2,l4))
title(main=names(n)[2])
plot(pcrfit(n[[3]], 1,2,l4))
title(main=names(n)[3])
plot(pcrfit(n[[4]], 1,2,l4))
title(main=names(n)[4])
plot(pcrfit(n[[5]], 1,2,l4))
title(main=names(n)[5])

norms <- list()

norms[[1]] <- n[[4]][[1]] #subtract background from foreground
for(k in 2:length(n[[4]])) {
     norms[[k]] <- (n[[4]][[k]] - n[[2]][[k]])
}

df <- data.frame(matrix(unlist(norms), nrow=30)) #create data frame for pcrfit

fluo1 <- list() #some shitty containers to hold that nonsense
eff <- list()

for(k in 2:length(df)) { 
     tryCatch({ 
     y <- pcrfit(df, 1, k, l4)
     x <- efficiency(y, plot=FALSE)
          
     fluo1[[k-1]] <- x$init1
     tt <- c(k, "/", length(norms))
     print(tt)
     print(x$init1)
     eff[[k-1]] <- x$eff
     }, error = function(err) {
#           y <- pcrfit(df, 1, k+1, l4)
#           x <- efficiency(y, plot=FALSE)
#           fluo1[[k-1]] <- as.numeric(x$init1)
#           print(tt)
#           print(x$init1)
     })
}
xt <- seq(1, length(unlist(fluo1)))

plot(xt, as.numeric(unlist(fluo1)), ylim=c(-5000,5000), ylab="Initial Fluorescence", xlab="Run Number")

smoothScatter(xt, as.numeric(unlist(fluo1)), ylim=c(-5000,5000), ylab="Initial Fluorescence", xlab="Run Number")

df1 <- data.frame(x=xt, y=as.numeric(unlist(fluo1)))
ggplot(as.data.frame(df1), aes(x=x, y=y)) + ylab("Initial Template Fluorescence") + xlab("Run Number") + ggtitle("F0 vs Run #") + geom_point(alpha = 0.4) + ylim(c(-5000,5000)) + geom_point()

