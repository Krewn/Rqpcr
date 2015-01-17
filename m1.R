#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
#############################################
m1 <- function(gotData){
     f0 <- list() #initial target quantity expressed in fluorescence units at cycle 0
     c <- list() #fractional cycle at which reaction fluorescence reaches half of fMax
     b <- list() #inflection point
     data <- list()
     bkgd <- list() #background data 
     fMax <- list() #maximal reaction fluorescence per run/35 cycles
     
     bkgd <- gotData[[1]][["Bkgd Data for Probe EvaGreen"]][2:length(gotData[[1]][["Bkgd Data for Probe EvaGreen"]])]
     
     data <- gotData[[1]][["Raw Data for Probe EvaGreen"]][2:length(gotData[[1]][["Raw Data for Probe EvaGreen"]])]
     
     for(k in 1:length(data)) { #get the max value of the run/35 cycles
          temp <- length(data[[k]])
          fMax[[k]] <- max(unlist(data[[k]][[3]]:data[[k]][[temp]]))
     }
     
     flag <- list()
     for(k in 1:length(data)) { #This loop + next calculates value of c
          flag[[k]] <- 1
          for(k2 in 3:length(data[[k]])) {
               if(k >= (as.integer(fMax[[k]])/2)) {
                    break
               }
               flag[[k]] <- as.integer(flag[[k]]) + 1
          }
     }
     
     for(k in 1:length(data)) { #assigns value of C
          c[[k]] <- (31) + ((fMax[[k]]/2) - as.integer(data[[k]][[31]]))/
               (as.integer(data[[k]][[32]]) - as.integer(data[[k]][[31]]))
     }
     
     for(k in 1:length(c)) {
          print(c[[k]])
     }
     #      for(k in 2:length(data)) {
     #           f0[k-1] <- (fMax[k-1]/(1+exp(c/b)))
     #      }
     
     #Fx = (Fmax/(1+exp(-(1/b)(x-c)))+Fb) #reaction fluorescence at cycle x
     return(0)
}