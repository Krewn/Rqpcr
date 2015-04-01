#CT calculations

#First derivative max
#http://www.ncbi.nlm.nih.gov/pubmed/15036369/
ctFDM <- function(data1) {
     return(which.max(diff(data1)))
}

#Second derivative max
#http://www.ncbi.nlm.nih.gov/pubmed/14530455/
ctSDM <- function(data1) {
     return(which.max(diff(diff(data1))))
}

#PMC method
#http://www.ncbi.nlm.nih.gov/pmc/articles/PMC52277/
ctPMC <- function(data1) {
     
}