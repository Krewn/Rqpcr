#############################################
#Packae is liscnesed by                     #
#  KrewnSolotions   /< /? [- \/\/ |\|       #
#############################################

readFiles <- function() { #gets all the files in directory ending in .csv, returns them as temp[x]
  temp = list.files(pattern="*.csv")
  #for (i in 1:length(temp)) print(temp[i])
  return(temp)
}

getData <- function() {
  files <- readFiles()
  dat <- list()
  for(i in 1:length(files)) {                 # Once for each .csv file in the cwd
    print(files[[i]])
    dat[[files[i]]] <- list()                 # Make a new entry with name corosponding to sorce file
    lines <- readLines(files[i])              # Get the file plain text
    experi <- grep("Experiment", lines)       # This identifies where the experimental info table starts
    k<-2                                
    dat[[files[i]]][['expInfo']] <- list()
    paste('','expInfo', sep='\t')
    
    while(substr(lines[experi+k],0,2)!="Ct") {
      temp <- strsplit(lines[experi+k],',',fixed = F , perl = F, useBytes = F)
      if(k<3){dat[[files[i]]][['expInfo']][k-1] <- list(unlist(temp)[0:(length(unlist(temp))-1)])}else{ 
              dat[[files[i]]][['expInfo']][k-1] <- temp}#removes the (empty) column user defined comments
      k<-k+1
    }
    
    Bkgd <- grep("Bkgd", lines)
    Raw <- grep("Raw", lines)
    
    for(k in Bkgd) {            		#get the background tables
      first <- T                   
      k2 <-0
      while(substr(lines[k+k2],0,1)=='S' | k2<2) {
        if(first) {
          q = lines[k]
          dat[[files[i]]][[q]]<-list()
          first <- F
        }
        dat[[files[i]]][[q]][k2]<- strsplit(lines[k+k2],',',fixed = F , perl = F, useBytes = F)
        k2<-k2+1    
      }
    }
    
    for(k in Raw) {             		#get the forground tables 
      first = T
      k2 <-0
      while(substr(lines[k+k2],0,1)=='S' | k2<2){
        if(first){
          q =lines[k]
          dat[[files[i]]][[q]]<-list()
          first <- F
        }
        dat[[files[i]]][[q]][k2] <- strsplit(lines[k+k2],',',fixed = F , perl = F, useBytes = F)
        k2<-k2+1    
      }      
    } 
  }
  return(dat)
}

squish <- function(data){		#converts a long list into a fat list
  table <- list()
  for(k in 1 : length(data)){
    if((data[[k]][[1]]%in%unlist(names(table)))== F){
      table[[data[[k]][[1]]]]<-list()
    }
    table[[data[[k]][[1]]]][[data[[k]][[2]]]]<-data[[k]][[3]]
  }
  return(table)
}

squish2<-function(data){
  table <- list()
  if(names(data) == NULL){
    if(length(data[[1]]) == 3){
      return(squish(data))
    }else{
      for(k in 1 : length(data)){
        tag<-strsplit(data[k],'_',fixed = F , perl = F, useBytes = F)
        if((tag[1]%in%unlist(names(table)))== F){
          table[[ tag[1] ]]<-list()
        }
        table[[ tag[1] ]][[ tag[2] ]]<-data[[k]][[3]]
      }
      return(table)
    }
  }else{
    for(k in names(data)){
      tag<-strsplit(k,'_',fixed = F , perl = F, useBytes = F)
      if((tag[1]%in%unlist(names(table)))== F){
        table[[tag[1]]]<-list()
      }
      table[[ tag[1] ]][[ tag[2] ]]<-data[[k]]
    }
    return(table)
  }
}

LOLprint <- function(LOL){			# list of list print
  ret <- ""
  First <- T
  for(k0 in LOL){
    temp <- ""
    first <- T
    for(k1 in k0){
      if(first){
        temp <- k1
        first <- F
      }else{
        temp <- paste(temp,k1,sep = ",",collapse = NULL)
      }
    }
    if(First){
      ret <- temp
      First<-F
    }else{ret <- paste(ret , temp ,sep = "\n", collapse = NULL)}
  }
  return(ret)
}

