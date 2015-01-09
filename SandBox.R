#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
source("./qpcRsources.R")                   #
#############################################

gd <- getData()
LOLprint(gd)
n <- gdToDf(gd)  
#for(k in n){print(c(k)}
pcrfit(as.data.frame(n[[3]]), cyc=1, 1, stat = NULL, offset= 0, weight =NULL ,verbose=TRUE)
plot(pcrfit(n[[2]], 1,2,l4))
title(main=names(n)[2])
plot(pcrfit(n[[3]], 1,2,l4))
title(main=names(n)[3])
plot(pcrfit(n[[4]], 1,2,l4))
title(main=names(n)[4])
plot(pcrfit(n[[5]], 1,2,l4))
title(main=names(n)[5])

