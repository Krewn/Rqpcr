#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #

#############################################

#http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3047581/
m6 <- function(data1){
     F0 <- list() #initial template fluorescence
     Eff <- list() #Linear regression of En versus Fn fitted to the largest possible window, defined by the difference to averaged F0 values
     EffMax <- list() #Max efficiency values
     Fmax <- list() #max value?
     start <- list()
     end <- list()
     
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
     
     for(k in 2:length(data1)) { #calculates efficiency for each cycle, and LRE window
          Eff[[k-1]] <- list()
          tempFlag <- 0
          first <- TRUE
          window <- FALSE
          for(k2 in 2:(length(data1[[k]]))) { #for(k2 in 2:length(data1[[k]])) {
               Eff[[k-1]][[k2-1]] <- ((data1[[k]][[k2]])/(data1[[k]][[k2-1]]) - 1)
               
               if((first == TRUE) && (Eff[[k-1]][[k2-1]] > .2)) { #find beginning of LRE window, efficiency > 1.2
                    start[[k-1]] <- (k2-1)
                    first <- FALSE
                    window <- TRUE
               }
               
               if(window == TRUE) { #if we are inside the LRE window check if we have hit the end, Eff < 1.3 = end
                    if(Eff[[k-1]][[k2-1]] < .2) {
                         end[[k-1]] <- (k2-1)
                    }
                    if(k2 == length(data1[[k]])) { #if we hit the end of the cycles then the last cycle is the end of the window
                         end[[k-1]] <- (k2-1)
                    }
               }
               
               if(Eff[[k-1]][[k2-1]] > tempFlag) { #Max efficiency
                    EffMax[[k-1]] <- Eff[[k-1]][[k2-1]]
                    tempFlag <- EffMax[[k-1]]
               }
               
               if(window == FALSE && k2 == length(data1[[k]])) { #if we never found a window then the efficienct sucked.
                    start[[k-1]] <- 1
                    end[[k-1]] <- 1
               }
          }
     }
     print("Done calculating efficiency")

     
     for(k in 1:length(EffMax)) {
          tempC <- (start[[k]] - end[[k]]) #how many cycles long is the LRE window
          tempCONT <- list()
          for(k2 in 1:length(tempC)) { #iterate through the LRE window and calculate the F0 values
               tempCONT[[k2]] <- Fmax[[k]]/(1+((Fmax[[k]]/data1[[k+1]][[start[[k]]+k2-1]]) - 1)*(EffMax[[k]]+1))^(start[[k]]+k2-1)
          }
          F0[[k]] <- mean(unlist(tempCONT)) #calculate the mean of the F0 values and store that value in F0, which is our F0 value
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
                    tempCont[[tempCounter+2]] <- paste(temp223, F0[[k]], sep=",")
                    smallFluo[[tempCounter]] <- F0[[k]]
                    tempCounter <- tempCounter + 1
               }
          }, error = function(err) {
          })
     } 
     
     writeLines(LOLprint(tempCont), "LRE-Emax.ddv")
     
     return(smallFluo)
     
}
