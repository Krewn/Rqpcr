#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
source("./qpcRsources.R")                   #
#############################################

gd <- getData()
#LOLprint(gd)
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

q <- list()
q <- n[[4]][2:length(n[[4]])] - n[[2]][2:length(n[[2]])]
q[[1]] <- n[[4]][[1]]

# q[[2]] <- c(610, 618, 627, 632, 628, 634, 634, 628, 634, 642, 637, 643, 653, 666, 684, 717, 
#             787, 923, 1197, 1716, 2638, 4077, 5461, 7007, 8561, 9994, 11278, 12382, 13382, 
#             14252)

# norms <- list()
# 
# norms[[1]] <- n[[4]][[1]] #subtract background from foreground
# for(k in 2:length(n[[4]])) {
#      norms[[k]] <- (n[[4]][[k]] - n[[2]][[k]])
# }

# m2(q) #run the second method with the normalized data
# Genes1 <<- list()
# fixSHIT <- function() {
#      tempGENES <- list()
#      t <- list() #info section chamber id
#      t2 <- list() #info section gene names
#      temp323 <- list() #temp323 = raw data values
#      for(k in 2:length(gd[[1]][[1]])) {
#           t[[k-1]] <- gd[[1]][[1]][[k]][[1]] 
#           t2[[k-1]] <- gd[[1]][[1]][[k]][[5]]
#      }
#      
#      t <- gsub("A", "D", t)
#      
#      for(k in 2:length(t)) { 
#           temp323[[k-1]] <- paste(gd[[1]][[2]][[k]][1], gd[[1]][[2]][[k]][2], sep="-")
#      }
#      
#      for(k in 1:(length(t)-1)) {
#           for(k2 in 1:length(t)) {
#                if(t[[k2]] == temp323[[k]]) {
#                     tempGENES[[k]] <- t2[[k]]
#                     print(tempGENES[[k]])
#                }
#           }
#           print(paste(k, "/", length(t)))
#      }
#      return(tempGENES)
# }
# 
# Genes1 <<- fixSHIT()

#m5(q)
m6(q, 30)






