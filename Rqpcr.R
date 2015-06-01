#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
#############################################

qpcRdf <- function (LOL, Headers = True){        # takes a 2d table, one of the sub tables (expects LOL to be a list of lists) 
  First = T
  t <- double()
  head<-double()
  q = 1
  for(k in LOL){                                  # for each row in the table
    temp <- unlist(k)                             #cast from list to vector
    #print(temp[3:length(temp)])
    t <- cbind(t,as.numeric(temp[3:length(temp)]))#Gets everything but the row label (when transposed these will be the column lable)
    head <- cbind(head,paste(temp[1],temp[2],sep="_",collapse=NULL))
    q<-q+1
  }
  #print(length(t))
  #print(length(c(head)))
  #t <- data.frame(t)
  colnames(t)<-unlist(head)
  return(as.data.frame(t))
}

gdToDf<-function(gotData,tableNum=1){
  print("Please Wait While your input is formatted.") # this could(should) happen in getData but....-Waisting your time-
  n<-list()
  c<-1
  Doe = names(gotData[[names(gotData)[[1]] ]])
  for(k in gotData[[names(gotData)[[tableNum]] ]]){
    n[[ Doe[[c]] ]]<-qpcRdf(k)                               #Creates Data frames from the varius tables.
    print(paste("   Table ",Doe[[c]]," format complete...",collapse=NULL,sep=''))
    c<-c+1
  }

  return(n)
}
