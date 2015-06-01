#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
#############################################

#http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3258155/

m8 <- function(data1){
     cat("Initializing m8 :")
     m8Cq <<- list()
     m8Qc <<- list()
     m8Quant <<- list()
     Cq <- list()
     Eff <- list()
     F0 <- list()
     for(k in 2:length(data1)) {
          cat(paste("\t",k,"/",length(data1)))
          tryCatch({ 
               Cq[[k-1]] <- which.max(diff(diff(data1[[k]]))) #second derviative maximum
               m8Cq[[k-1]] <<- which.max(diff(diff(data1[[k]])))
               Eff[[k-1]] <- data1[[k]][[Cq[[k-1]]]]/data1[[k]][[Cq[[k-1]]-1]]
               F0[[k-1]] <- (data1[[k]][[Cq[[k-1]]]])/((Eff[[k-1]])^Cq[[k-1]]) 
               
               m8Quant[[k-1]] <<- F0[[k-1]]
               
               if(max(diff(diff(data1[[k]]))) > 50) {
                    m8Qc[[k-1]] <<- "Pass"
               } else {
                    m8Qc[[k-1]] <<- "Fail"
               }
               if(FALSE){ #Ghetto block comment
               if(k == 10) {
                    print(data1[[k]])
                    print(Cq[[k-1]])  
                    print(F0[[k-1]])
               }
               }
          }, error = function(err) {
               F0[[k-1]] <- 1
               m8Cq[[k-1]] <<- 0
               m8Qc[[k-1]] <<- "Fail"
               m8Quant[[k-1]] <<- 0
          })
          cat("\r")
     }
     
     #printSHIT(F0,"Method: 5PSM", paste("Chamber ID", "Initial Template Fluorescence", sep=","), "5PSM_FULL.ddv")
     
#      tempCont <- list()
#      tempCounter <- 1
#      smallFluo <- list()
#      
#      tempCont[[1]] <- "Method: 5PSM"
#      tempCont[[2]] <- paste("Chamber ID", "Initial Template Fluorescence")
#      for(k in 1:length(F0)) {
#           tryCatch({ 
#                if(CT[[k]] > 0) {
#                     temp223 <- paste(gd[[1]][[2]][[k+2]][1], gd[[1]][[2]][[k+2]][2], sep="-")
#                     tempCont[[tempCounter+2]] <- paste(temp223, F0[[k]], sep=",")
#                     smallFluo[[tempCounter]] <- F0[[k]]
#                     tempCounter <- tempCounter + 1
#                }
#           }, error = function(err) {
#           })
#      } 
#      
#      writeLines(LOLprint(tempCont), "5PSM.ddv")
     
     return(list("F0"=F0,"Qc"= m8Qc))
}
