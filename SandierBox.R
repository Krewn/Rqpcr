print("Reading Data")

Maingd <- getData()#data input
MainN <- gdToDf(Maingd)#data format

MainProbe <- MainN[[4]]- MainN[[2]]
MainProbe[[1]] <- MainN[[4]][[1]]

#rox <- n[[5]]- n[[3]]
#rox[[1]] <- n[[5]][[1]]

#rox <- n[[5]]- n[[3]]
scfF0s = 0
source("./m1.R")
scfF0s <- list()
scfF0s <- m1(MainProbe)
print(scfF0s)
cy0s <- list()
cy0s <- m1cyo(MainProbe)
print(cy0s)