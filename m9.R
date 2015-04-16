
#http://www.ncbi.nlm.nih.gov/pmc/articles/PMC219490/
m9 <- function(data1) {
     F0 <- list()
     
     for(k in 2:length(data1)) {
          tryCatch({
               start <- 6 
               end <- which.max(diff(diff(data1[[k]])))
               Eff <- list()
               EffM <- 0
               
               for(k2 in 2:(length(data1[[k]]))) {
                    if((data1[[k]][[k2]] / data1[[k]][[k2-1]]) > 1.1)
                    start = k2-1
               }
               
               Fx <- list()
               
               if(start > end) {
                    end = start+1
               }
               
               for(k2 in start:end) {
                    Eff[[(k2-start+1)]] <- data1[[k]][[k2]]/data1[[k]][[k2-1]]
               }
               
               Eff <- as.vector(Eff, "numeric")
               EffM <- mean(Eff)
               
               F0[[k-1]] <- 1/(EffM)^((start+end)/2)
               
     #           Fx[[k-1]] <- list()
     #           
     #           max <- max(data1[[k]])
     #           y0 <- data1[[k]][[1]]
     #           x0 <- which.max(diff(data1[[k]]))
     #           b <- data1[[k]][[x0+1]] - data1[[k]][[x0]] 
     #           a <- max-y0
     #           SDM <- which.max(diff(diff(data1[[k]])))
     #           
     #           for(k2 in 1:SDM) {
     #                Fx[[k-1]][[k2]] <- y0 + a/(1 + (k2/x0)^(b))
     #               # Fx[[k-1]][[k2]] <- max/(1 + exp(-b*(k2-x0)))
     #           }
               
               if(k == 10) {
                    print(F0[[k-1]])
                    print(start)
                    print(end)
                    print(EffM)
               }
          }, error = function(err) {
               F0[[k-1]] = 0
          })
     }
     
     printSHIT(F0,"Method: PCR-Miner", paste("Chamber ID", "Initial Template Fluorescence", sep=","), "PCR-MinerFULL.ddv")
     
     tempCont <- list()
     tempCounter <- 1
     smallFluo <- list()
     
     tempCont[[1]] <- "Method: PCR-Miner"
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
     
     writeLines(LOLprint(tempCont), "PCR-Miner.ddv")
     
     return(smallFluo)
}






