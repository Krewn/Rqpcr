#############################################
#Packae is liscnesed b+v                    #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
#############################################

#http://www.biomedcentral.com/1471-2105/9/221 - (Uses the qpcR fitting library, http://www.inside-r.org/packages/cran/qpcR/docs/pcrfit)
#sigmoidal curve type for qPCR analysis, the four-parameter logistic curve (also termed Boltzmann fit)(Boltzmann fit - http://www.graphpad.com/guides/prism/6/curve-fitting/index.htm?reg_classic_boltzmann.htm)
#additionally its four-parameter log-logistic counterpart to five-parameter versions that were shown to exhibit better fits for asymmetric data
#choosing the best model from nested f tests
m2 <- function(df){
     fluo1 <- list() #initial template fluorescence
     eff <- list() #efficiency
     ct <- list() #ct values, 'threshold cycle', according to Guescini
     r2 <- list() #R^2 value
     tempFLUO <- list()
     ccount <- 1
     
     for(k in 2:length(df)) { 
          tryCatch({ +
               y <- pcrfit(df, 1, k, l4)
               x <- efficiency(y, plot=FALSE, type="Cy0") #type = method of efficiency estimation
               
               fluo1[[k-1]] <- x$init1
               tt <- c(k, "/", length(norms))
               print(tt)        #Cy0 = the intersection of a tangent on the first derivative maximum with the abscissa as calculated according to Guescini
               eff[[k-1]] <- x$eff
               ct[[k-1]] <- x$Cy0
               r2[[k-1]] <- x$Rsq
               
#                if(as.integer(ct[[k-1]]) > 0) {
#                     tempFLUO[[ccount]] <- fluo1[[k-1]]
#                     print(tempFLUO[[ccount]])
#                     ccount <- ccount + 1
#                }
          }, error = function(err) {
          })
     }
#      m2Print(fluo1, gd, ct, r2)
     
     #optional plots
     fluo2 <- list()
     ts <- 1
     for(k in 1:length(fluo1)) {
          if(fluo1[[k]] > 0) {
               fluo2[[ts]] <- fluo1[[k]]
               ts <- ts + 1
          }
     }

     xt <- seq(1, length(unlist(fluo2)))
     plot(xt, as.numeric(unlist(fluo2)), ylim=c(-1000,2000), ylab="Initial Fluorescence", xlab="Run Number")
     
#      smoothScatter(xt, as.numeric(unlist(fluo2)), ylim=c(0,2500), ylab="Initial Fluorescence", xlab="Run Number")
#      
#      df1 <- data.frame(x=xt, y=as.numeric(unlist(fluo1)))
#      ggplot(as.data.frame(df1), aes(x=x, y=y)) + ylab("Initial Template Fluorescence") + xlab("Run Number") + ggtitle("F0 vs Run #") + ylim(c(-5000,5000)) + geom_point(alpha=0.5)
#      
}


m2Print <- function(fluo, gd, ct, r2) {
     temp <- list()
     temp[[1]] <- "ROX_Plate_3, EvaGreen"
     temp[[2]] <- "Chamber ID, Initial Template Fluorescence, Ct value(according to Guescini), efficiency, R^2 value"
     for(k in 1:length(fluo)) {
          temp2 <- paste(gd[[1]][[2]][[k+1]][1], gd[[1]][[2]][[k+1]][2], sep="-")
          temp[[k+1]] <- paste(temp2, gd[[1]][[1]][[k+1]][[5]], fluo[[k]], ct[[k]], r2[[k]])
     }
     writeLines(LOLprint(temp),"test1.ddv")
}
