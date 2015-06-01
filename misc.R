#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
#############################################

askYesNo <- function() {
  n <- readline("Would you like to save the image? (Y/N):")
  if(n == 'Y' | n == 'y' | n == 'yes' | n == 'Yes') {
    print("Image saved")
    return(1)
  } else {
    print("Image not saved")
    return(0)
  }
}

#Lol what is this function even here for?
ptf <- function (txtToPrint,outFile){system(paste("echo '",cat(txtToPrint),"'>",outFile,sep='',collapse=NULL))}

findInflections <- function(data) {
     x <- seq(1,30)
     for(k in 2:length(data)) {
          y <- as.integer(data[[k]][3:32])
          infl <- c(FALSE, diff(diff(y)>0)!=0)
          
          plot(x,y,type="l")
          points(x[infl ], y[infl ], col="blue")
     }
}
