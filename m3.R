#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
#############################################

#http://www.ncbi.nlm.nih.gov/pubmed?Db=PubMed&Cmd=ShowDetailView&TermToSearch=20814578
m3 <- function(data1) {
     cycles <- list() #list of cycles preceding second derivative maximum, because mak2 only models a partial curve
     Eff <- list() #efficiency, ((Dn - D(n-1))/D(n-1))
     
     for(k in 2:length(data1)) { #chop off values after second derivative max
          cycles[[k-1]] <- list()
          cycles[[k-1]] <- data1[[k]][1:which.max(diff(diff(data1[[k]])))]
     }
     
     
     
#      for(k in 2:length(data1)) { #calculate efficiency values
#           Eff[[k-1]] <- list()
#           for(k2 in 1:length(data1)) {
#                Eff[[k-1]][[k2]] <- (data1[[k]][[k2+1]]-data1[[k]][[k2]])/(data1[[k]][[k2]])
#           }
#      }
}
