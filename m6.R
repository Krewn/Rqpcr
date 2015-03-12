#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #

#############################################

#http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3047581/
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
          tempFlag <- 0
          for(k2 in 2:8) { #for(k2 in 2:length(data1[[k]])) {
               Eff[[k-1]][[k2-1]] <- ((data1[[k]][[k2]])/(data1[[k]][[k2-1]]) - 1)
               if(Eff[[k-1]][[k2-1]] > tempFlag) {
                    EffMax[[k-1]] <- Eff[[k-1]][[k2-1]]
                    tempFlag <- EffMax[[k-1]]
               }
          }
     }
     print("Done calculating efficiency")
     print(length(EffMax))
     
     for(k in 1:length(EffMax)) {
          F0[[k]] <- Fmax[[k]]/(1+((Fmax[[k]]/data1[[k+1]][[c]]) - 1)*(EffMax[[k]]+1))^c
     }
     
     printSHIT(F0,"Method: LRE-Emax", paste("Chamber ID", "Initial Template Fluorescence", sep=","), "LRE-EmaxFULL.ddv")
     
     tempCont <- list()
     tempCounter <- 1
     smallFluo <- list()
     
     tempCont[[1]] <- "Method: LRE-Emax"
     tempCont[[2]] <- paste("Chamber ID", "Initial Template Fluorescence")
     for(k in 1:length(F0)) {
          tryCatch({ 
               if(CT[[k]] > 0) {
                    temp223 <- paste(gd[[1]][[2]][[k+2]][1], gd[[1]][[2]][[k+2]][2], sep="-")
                    tempCont[[tempCounter+2]] <- paste(temp223, F0[[k+1]], sep=",")
                    smallFluo[[tempCounter]] <- F0[[k+1]]
                    tempCounter <- tempCounter + 1
               }
          }, error = function(err) {
          })
     } 
     
     writeLines(LOLprint(tempCont), "LRE-Emax.ddv")
     
     return(smallFluo)
     
}
