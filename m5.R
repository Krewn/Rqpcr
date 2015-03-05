#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
#############################################

#This may well be piped in through some bash action...
#The LRE Analyzer -- Full on Java application
#http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2716216/


#MY NOTES - Pass Function q list, not a dataframe, fuck that shit
temp2 <- list()
m5 <- function(data1){
  Efficiency <- list() #Efficiency, Linear regression fitted on log of F in a window of 4 cycles, that delivers the least sigma(E) between reactions per amplicon
  Eff <- list() #amplification efficiency
  F0 <- list() #initial fluo. at x=0, F0 = (Fq)/(E^(Cq))
  Fq <- list() #Fluo. at Cq

  for(k in 2:length(data1)) { #calculates efficiency for each cycle, which is fold increase in PCR product per cycle
       Eff[[k-1]] <- list()
       for(k2 in 2:length(data1[[k]])) {
            Eff[[k-1]][[k2-1]] <- (data1[[k]][[k2]])/(data1[[k]][[k2-1]])
       }
  }
  
  for(k in 1:length(Eff)) {
       Efficiency[[k]] <- sDeviation(Eff[[k]], 4)
       temp2[[k]] <- Cq
#        print(Efficiency[[k]])
  }
  
  for(k in 1:length(Efficiency)) {
       F0[[k]] <- as.integer(data1[[k]][[temp2[[k]]]])/(as.integer(Efficiency[[k]]^temp2[[k]]))
#        print(F0[[k]])
  }

   printSHIT(F0,"Method: LinRegPCR", paste("Chamber ID", "Gene", "Initial Template Fluorescence", sep=","))
#    plotSHIT(F0)

}

sDeviation <- function(values, width) {
     tst <- 10000000
     Cq <<- 3 #Cq = Quantification cycle, One cycle below the top border  of the best window of cycles
     E1 <-0
     
     for(k in 1:7) { #for(k in 1:(length(values)-3)) {
          val <- list()
          for(k2 in 1:width) {
               temp3 <- k+k2-1
               val[[k2]] <- values[[temp3]]
          }
          val <- as.vector(val, "numeric")
          if(sd(val) < tst) {
               Cq <<- (k+2)
               tst <- sd(val)
               E1 <- mean(val)
          }
     }
     return(E1)
}

printSHIT <- function(thingy, method, columns, name) {
     container <- list()
     container[[1]] <- method
     container[[2]] <- columns
     for(k in 1:length(thingy)) {
          temp223 <- paste(gd[[1]][[2]][[k+1]][1], gd[[1]][[2]][[k+1]][2], sep="-")
          container[[k+2]] <- paste(temp223, thingy[[k]], sep=",")
     }
     writeLines(LOLprint(container), name)
}

plotSHIT <- function(thingy) {
     xt <- seq(1, length(unlist(thingy)))
     
     plot(xt, as.numeric(unlist(thingy)), ylim=c(0,2000), ylab="Initial Fluorescence", xlab="Run Number")
     smoothScatter(xt, as.numeric(unlist(thingy)), ylim=c(0,2500), ylab="Initial Fluorescence", xlab="Run Number")
          
}

