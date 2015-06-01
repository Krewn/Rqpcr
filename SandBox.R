#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
source("./qpcRsources.R")                   #
#############################################
setwd(file.path(".","FluoData"))
gd <- getData()

if(F){ ## grab tables for for Hiam's code
  haimRox   <- LOLprint(gd[[names(gd)[[1]]]][["Raw Data for Passive Reference ROX" ]])
  haimProbe <- LOLprint(gd[[names(gd)[[1]]]][["Raw Data for Probe EvaGreen"]])
  haimKey   <- LOLprint(gd[[names(gd)[[1]]]][["expInfo"]])
  TempFileHandle <- 0
  
  mainDir <- "."
  subDir <- "haimDir"
  
  haimDirs = list(rox = file.path(".",subDir,"hRox_TempFile.csv"))
  
  rmMkdir(file.path(mainDir, subDir))
  
  rmTouch(file.path(".",subDir,"hRox_TempFile.csv"))
  writeLines(haimRox,file.path(".",subDir,"hRox_TempFile.csv"))
  
  rmTouch(file.path(".",subDir,"hProbe_TempFile.csv"))
  writeLines(haimProbe,file.path(".",subDir,"hProbe_TempFile.csv"))
  
  rmTouch(file.path(".",subDir,"hKey_TempFile.csv"))
  writeLines(haimKey,file.path(".",subDir,"hKey_TempFile.csv"))
  
  hs(file.path(".",subDir,"hRox_TempFile.csv"),
     file.path(".",subDir,"hProbe_TempFile.csv"),
     file.path(".",subDir,"hKey_TempFile.csv"))
}

CT <<- list()
chamberId <<- list()
geneName <<- list()
ctCall <<- list()
peakCall <<- list()

print("Thankyou for computing, please wait while I work on this for you.")

for(NumeroDeTable in 1:length(gd)){ # Itterate over all CSV files found by
  n <- gdToDf(gd,tableNum=NumeroDeTable)
  q <- n[[4]][1:length(n[[4]])] - n[[2]][1:length(n[[2]])]
  q[[1]] <- n[[4]][[1]]
  
  tempCo <- list()
  values <- list()
  posCt <- list() #ct values where something was detected
  if(F){
    for(k in 2:length(gd[[1]][[4]])) {
      tempCo[[k-1]] <- paste(gd[[1]][[4]][[k]][[1]], gd[[1]][[4]][[k]][[2]], sep="-")
    }
    silly = list()
    for (k in 2 : length(gd[[1]][[4]]) ){
      t <- gsub("A", "D", gd[[1]][[1]][[k]][[1]])
      silly[[t]] <- list(
        v = gd[[1]][[1]][[k]][[7]],
        c = gd[[1]][[1]][[k]][[1]],
        g = gd[[1]][[1]][[k]][[5]],
        ct = gd[[1]][[1]][[k]][[9]],
        p  = gd[[1]][[1]][[k]][[14]]
      )
    }
    for(k in 2:length(tempCo)) {
      values[[k]] <- silly[[tempCo[[k]]]][['v']]
      chamberId[[k]] <<- silly[[tempCo[[k]]]][['c']]
      geneName[[k]] <<- silly[[tempCo[[k]]]][['g']]
      ctCall[[k]] <<- silly[[tempCo[[k]]]][['ct']]
      peakCall[[k]] <<- silly[[tempCo[[k]]]][['p']]
    }
}
  
  m1Fluo <- list()
  m5Fluo <- list()
  m6Fluo <- list()
  #m7Fluo <- list()
  m8Fluo <- list()
  
  m1Fluo <- m1(q) #  return list("F0" = scflist, "Qc" = cy0list )
  m5Fluo <- m5(q) #  return list("F0" = F0     , "Qc" = m5Qc    )
  m6Fluo <- m6(q) #  return list("F0" = F0     , "Qc" = m6Qc    )
  #m7Fluo <- m7(q) #  return list("F0" = m7Quant, "Qc" = m7Qc    )
  m8Fluo <- m8(q) #  return list("F0" = F0     , "Qc" = m8Qc    )
  
  Opath = file.path(".", paste(strsplit(names(gd)[NumeroDeTable],".",fixed= T)[[1]][1],"opDATA",sep="") )
  rmMkdir(Opath)
  
  F0QcOutPut(m1Fluo,Opath,n,"m1")  #  return list("F0" = F0     , "Qc" = m8Qc    )
  F0QcOutPut(m5Fluo,Opath,n,"m5")
  F0QcOutPut(m6Fluo,Opath,n,"m6")
  #F0QcOutPut(m7Fluo,Opath,n,"m7")
  F0QcOutPut(m8Fluo,Opath,n,"m8")
}
  
  
# TODO make list of all methods with good names...

# #m6Fluo <- m6(q)


# plot(log(unlist(m7Fluo)), log(unlist(m5Fluo)), xlab="Cy0 Method", ylab="LinRegPCR")
# plot(log(unlist(m7Fluo)), log(unlist(m6Fluo)), xlab="Cy0 Method", ylab="LRE-Emax")
# plot(log(unlist(m7Fluo)), log(unlist(m8Fluo)), xlab="Cy0 Method", ylab="5PSM")
# plot(log(unlist(m9Fluo)), log(unlist(m7Fluo)), xlab="LinRegPCR", ylab="5PSM")

printCq <- function() {
     bCt <- list()
     m5Ct <- list()
     m8Ct <- list()
     m9Ct <- list()
     tC <- list()
     counter <- 1
     for(k in 1:(length(tempCo) - 1)) {
          #if((m8Cq[[k]] > 15) && values[[k]] < 999) {
          if(m5Cq[[k]] > 3) {
               tC[[counter]] <- paste(tempCo[[k]],
                                      values[[k]],
                                      m8Cq[[k]],
                                      m5Cq[[k]],
                                      m9Cq[[k]],
                                      sep=",")
               m5Ct[[counter]] <- m5Cq[[k]]
               m8Ct[[counter]] <- m8Cq[[k]]
               m9Ct[[counter]] <- m9Cq[[k]]
               bCt[[counter]] <- values[[k]]
               counter <- counter + 1
          }
     }
     
#      plot(log(as.numeric(bCt)), log(as.numeric(m8Ct)))
#      #plot(log(as.numeric(unlist(bCt))), log(as.numeric(unlist(m9Ct))))
#      #plot(log(as.numeric(bCt)), log(as.numeric(m5Ct)))
#      plot(log(unlist(m9Ct)), log(unlist(m8Ct)))
     
     writeLines(LOLprint(tC), "Cq.ddv")
}
