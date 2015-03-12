

m1c <- function(data1) {
     M <- list() #Midpoint, M = Rnoise x sqrt(Rmax/Rnoise)
     Rmax <- list() #Maximum fluo.
     Rnoise <- list() #standard deviation of cycles 1-10
     Eff <- list()
     Fq <- list()
     
#      for(k in 2:length(data1)) { #find Rmax values
#           Rmax[[k-1]] <- max(data1[[k-1]][1:11])
#      }
#      print(Rmax[[10]])
#      
#      for(k in 2:length(data1)) { #find Rnoise values
#           Rnoise[[k-1]] <- sd(data1[[k-1]][1:10])
#      } 
#      
#      print(Rnoise[[10]])
#      
#      for(k in 1:length(Rmax)) {
#           M[[k]] <- Rnoise[[k]] * sqrt(Rmax[[k]]/Rnoise[[k]])
#      }
#      
#      print(M[[10]])
     
  
     E <- list()
     X0 <- list()
}