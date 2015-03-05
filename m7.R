#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
#############################################

m7 <- function(df){
     fluo1 <- list() #initial template fluorescence
     eff <- list() #efficiency
     ct <- list() #ct values, 'threshold cycle', according to Guescini
     r2 <- list() #R^2 value
     tempFLUO <- list()
     ccount <- 1
     
     for(k in 2:length(df)) { 
          tryCatch({ 
               y <- pcrfit(df, 1, k, l4)
               x <- efficiency(y, plot=FALSE, type="Cy0") #type = method of efficiency estimation
               
               fluo1[[k-1]] <- x$init1
               tt <- c(k, "/", length(df))
               print(tt)        #Cy0 = the intersection of a tangent on the first derivative maximum with the abscissa as calculated according to Guescini
               eff[[k-1]] <- x$eff
               ct[[k-1]] <- x$Cy0
               r2[[k-1]] <- x$Rsq
               CT[[k-1]] <<- ct[[k-1]]
               
               #                if(as.integer(ct[[k-1]]) > 0) {
               #                     tempFLUO[[ccount]] <- fluo1[[k-1]]
               #                     print(tempFLUO[[ccount]])
               #                     ccount <- ccount + 1
               #                }
          }, error = function(err) {
          })
     }
     m7Print(fluo1, gd, ct, r2)
     
     tempCont <- list()
     tempCounter <- 1
     smallFluo <- list()
     
     tempCont[[1]] <- "Method: Cy0"
     tempCont[[2]] <- paste("Chamber ID", "Initial Template Fluorescence")
     for(k in 1:length(fluo1)) {
          tryCatch({ 
               if(CT[[k]] > 0) {
                    temp223 <- paste(gd[[1]][[2]][[k+2]][1], gd[[1]][[2]][[k+2]][2], sep="-")
                    tempCont[[tempCounter+2]] <- paste(temp223, fluo1[[k]], sep=",")
                    smallFluo[[tempCounter]] <- fluo1[[k]]
                    tempCounter <- tempCounter + 1
                    print(smallFluo[[tempCounter-1]])
               }
          }, error = function(err) {
          })
     } 
     
     writeLines(LOLprint(tempCont), "Cy0.ddv")
     
     return(smallFluo)
     
     #      xt <- seq(1, length(unlist(fluo1)))
     #      plot(xt, as.numeric(unlist(fluo2)), ylim=c(-1000,2000), ylab="Initial Fluorescence", xlab="Run Number")
     
     #      smoothScatter(xt, as.numeric(unlist(fluo1)), ylim=c(0,2500), ylab="Initial Fluorescence", xlab="Run Number")
     #      
     #      df1 <- data.frame(x=xt, y=as.numeric(unlist(fluo1)))
     #      ggplot(as.data.frame(df1), aes(x=x, y=y)) + ylab("Initial Template Fluorescence") + xlab("Run Number") + ggtitle("F0 vs Run #") + ylim(c(-5000,5000)) + geom_point(alpha=0.5)
     #      
}


m7Print <- function(fluo, gd, ct, r2) {
     temp <- list()
     temp[[1]] <- "ROX_Plate_3, EvaGreen"
     temp[[2]] <- "Chamber ID, Initial Template Fluorescence, Ct value(according to Guescini), efficiency, R^2 value"
     for(k in 1:length(fluo)) {
          temp2 <- paste(gd[[1]][[2]][[k+1]][1], gd[[1]][[2]][[k+1]][2], sep="-")
          temp[[k+1]] <- paste(temp2, gd[[1]][[1]][[k+1]][[5]], fluo[[k]], ct[[k]], r2[[k]])
     }
     writeLines(LOLprint(temp),"Cy0FULL.ddv")
}