source("./qpcRsources.R")  

gd <- getData()

print(gd[[1]][[1]][[2]][5]) #Aldh1a2

unknown <- list()


for(k in 1:length(gd[[1]][[1]])) {
     unknown[[k]] <- gd[[1]][[1]][k+1]
}

print(paste(gd[[1]][[2]][[2]][1], gd[[1]][[2]][[2]][2], sep="-"))
     