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

q <- list()
q <- n[[4]][2:length(n[[4]])] - n[[2]][2:length(n[[2]])]
q[[1]] <- n[[4]][[1]]

# q[[2]] <- c(610, 618, 627, 632, 628, 634, 634, 628, 634, 642, 637, 643, 653, 666, 684, 717, 
#             787, 923, 1197, 1716, 2638, 4077, 5461, 7007, 8561, 9994, 11278, 12382, 13382, 
#             14252)

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
m5Fluo <- list()
m6Fluo <- list()
m7Fluo <- list()

m7Fluo <- m7(q)
m5Fluo <- m5(q)
m6Fluo <- m6(q, 30)

# m7Fluo[[911]] <- NULL

plot(log(unlist(m7Fluo)), log(unlist(m5Fluo)), xlab="Cy0 Method", ylab="LinRegPCR")
plot(log(unlist(m7Fluo)), log(unlist(m6Fluo)), xlab="Cy0 Method", ylab="LRE-Emax")
