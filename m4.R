#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
#############################################

#http://www.ncbi.nlm.nih.gov/pubmed/22102586
m4 <- function(data1){
     Fmax <- list() #max fluo value
     baseF <- list() #base fluo. value
     nFlex <- list() #inflection point of curve
     b <- list() #slope of the curve at nFlex
     Fn <- list() #list of lists for fit values
     
     for(k in 2:length(data1)) {
          Fmax[[k-1]] <- max(data1[[k]])
          baseF[[k-1]] <- data1[[k]][[1]]
          nFlex[[k-1]] <- which.max(diff(diff(data1[[k]])))
          b[[k-1]] <- (data1[[k]][[nFlex[[k-1]]]])/(data1[[k]][[nFlex[[k-1]] + 1]])
          
          Fn[[k-1]] <- list()
          for(k2 in 1:length(data1[[k]])) {
               Fn[[k-1]][[k2]] <- baseF[[k-1]] + ((Fmax[[k-1]] - baseF[[k-1]])/(1 + (k2/nFlex[[k-1]])^b[[k-1]]))
          }
          
          if(k==10) {
               xtr <- seq(1, length(Fn[[k-1]]))
               print(data1[[k]])
               plot(data1[[k]])
               lines(xtr, Fn[[k-1]])
          }
     }
}                        
