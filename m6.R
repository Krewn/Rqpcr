#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
#############################################

#  Evaluation was executed through sigma curve software.
#To fit the exponential part of the curve, optimally, we need to know exactly where the exponential phase of the PCR ends. Previous work suggested that the first positive second derivative maximum... 
#< comparison of linear and non-linear fits. < this should be fun to tackle > >
#  http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2665230/  
m6 <- function(data1, cCount){
     F0 <- list() #initial template fluorescence
     Eff <- list() #Linear regression of En versus Fn fitted to the largest possible window, defined by the difference to averaged F0 values
     EffMax <- list() #Max efficiency values
     Fmax <- list() #max value?
     c <- cCount #cycle count
     for(k in 2:length(data1)) { #get Fmax value
          tempFlag <- 0
          for(k2 in 1:length(data1[[k]])) {
               if(data1[[k]][[k2]] > tempFlag) {
                    tempFlag <- data1[[k]][[k2]]
                    Fmax[[k-1]] <- data1[[k]][[k2]]
               }
          }
     }
     print("Done Fmax")
     
     for(k in 2:length(data1)) { #calculates efficiency for each cycle
          Eff[[k-1]] <- list()
          tempFlag <- -1
          for(k2 in 2:length(data1[[k]])) {
               Eff[[k-1]][[k2-1]] <- ((data1[[k]][[k2]])/(data1[[k]][[k2-1]]) - 1)
               if(Eff[[k-1]][[k2-1]] > tempFlag) {
                    EffMax[[k-1]] <- Eff[[k-1]][[k2-1]]
                    tempFlag <- EffMax[[k-1]]
               }
          }
     }
     print("Done calculating efficiency")
     
     for(k in 1:length(EffMax)) {
          F0[[k]] <- Fmax[[k]]/(1+((Fmax[[k]]/data1[[k+1]][[c]]) - 1)*(EffMax[[k]]+1))^c
     }
     
     printSHIT(F0,"Method: LRE-Emax", paste("Chamber ID", "Initial Template Fluorescence", sep=","), "Method6.ddv")

}
