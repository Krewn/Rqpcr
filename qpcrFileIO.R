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

rmTouch <- function(path){
  if(file.exists(path)){
    file.remove(path)
  }
  file.create(path)
}

rmMkdir<- function(path){
  if(file.exists(path)){
    unlink(path, recursive=TRUE)
    file.remove(path)
  }
  dir.create(path)
}

F0QcPrint <- function (data,Location){
  colNames = names(data)
  op<-cbind(data[[colNames[[1]] ]],data[[colNames[[2]] ]])
  rmTouch(Location)
  prt = ""
  cat(length(op[1,]))
  cat("\n")
  cat(length(op[,1]))
  for(k in 1:length(op[1,])){
    for(k2 in 1:length(op[,1])){
      prt = paste(prt , op[k2,k] , sep=",")
    }
    prt = paste(prt, "\n" ,sep="")
  }
  writeLines(prt,Location)
}

F0QcOutPut <- function (data,Location,n,name){
  
  nVector <- names(n[[names(n)[[5]]]])[2:length(names(n[[names(n)[[5]]]]))]
  OutTable <- list()
  colNames <- cbind()
  
  for(k in 1:length(nVector)){
    nVector[[k]]<-strsplit(nVector[[k]],"_")
    nVector[[k]]<-unlist(nVector[[k]])
    if(nVector[[k]][[2]]=="D01"){
      OutTable[[ nVector[[k]][[1]] ]] <- list()
    }
    OutTable[[ nVector[[k]][[1]] ]][[nVector[[k]][[2]]]]<-0
  }
  OutTable2 <- OutTable
  
  for(k in 1:length(nVector)){
    OutTable [[nVector[[k]][[1]] ]][[nVector[[k]][[2]] ]] <- data[[names(data)[[1]] ]][[k]]
    OutTable2[[nVector[[k]][[1]] ]][[nVector[[k]][[2]] ]] <- data[[names(data)[[2]] ]][[k]]
  }
  
  OutTable <- mkStrOpTable(OutTable)
  OutTable2 <- mkStrOpTable(OutTable2)
  
  rmTouch(file.path(Location,paste(name,"F0",".csv",sep="")))
  writeLines(OutTable,file.path(Location,paste(name,"F0",".csv",sep="")))
  rmTouch(file.path(Location,paste(name,"Qc",".csv",sep="")))
  writeLines(OutTable2,file.path(Location,paste(name,"Qc",".csv",sep="")))
}

mkStrOpTable <- function(opTable){
  rowName <- names(opTable)
  colName <- names(opTable[[ rowName[1] ]])
  retStr <- ""
  for(k in 1:length(colName)){
    retStr<-paste(retStr,colName[[k]],sep=",")
  }
  for(k in 1:length(colName)){
    for(k2 in 1 : length(rowName)){
      if(k2==1){
        retStr<- paste(retStr,"\n",rowName[k],sep="")
      }
      retStr<- paste(retStr,opTable[[rowName[k] ]][[colName[k2] ]],sep=",")
    }
  }
  return(retStr)
}


if(F){
  fixThisShit <- function() {
    for(k in 2:length(gd[[1]][[4]])) {
      tempCo[[k-1]] <- paste(gd[[1]][[4]][[k]][[1]], gd[[1]][[4]][[k]][[2]], sep="-")
    }
    
    for(k in 1:length(tempCo)) {
      for(k2 in 2:length(gd[[1]][[4]])) {
        t <- gsub("A", "D", gd[[1]][[1]][[k2]][[1]])
        if(t == tempCo[[k]]) {
          values[[k]] <- gd[[1]][[1]][[k2]][[7]]
          chamberId[[k]] <<- gd[[1]][[1]][[k2]][[1]]
          geneName[[k]] <<- gd[[1]][[1]][[k2]][[5]]
          ctCall[[k]] <<- gd[[1]][[1]][[k2]][[9]]
          peakCall[[k]] <<- gd[[1]][[1]][[k2]][[14]]
          #print(values[[k]])
        }
      }
      print(paste(k,9217,sep="/"))
    }    
  }
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

printME <- function() {
  tempContainer2 <- list()
  print("Binding")
  tempContainer2[[1]] <- paste("Chamber ID", "Gene Name", "Ct Call" , "Peak Call","Sigmoidal Initial","Richard's Cy0", "LinRegPCR","LinRegPCR QC", "LRE-Emax", "LRE-Emax QC", "Cy0","Cy0 Qc", "5PSM" ,"5PSM Qc", sep=",")
  for(k in 2:length(chamberId)) {
    tempContainer2[[k]] <- paste(chamberId[[k]],
                                 geneName[[k]],
                                 ctCall[[k]],
                                 peakCall[[k]],
                                 m1Fluo[["F0"]][[k-1]],
                                 m1Fluo[["F0"]][[k-1]], 
                                 m5Fluo[['F0']][[k-1]],
                                 m5Fluo[['Qc']][[k-1]],
                                 m6Fluo[["F0"]][[k-1]],
                                 m6Fluo[["Qc"]][[k-1]],
                                 m7Fluo[["F0"]][[k-1]],
                                 m7Fluo[["Qc"]][[k-1]],
                                 m8Fluo[["F0"]][[k-1]],
                                 m8Fluo[["Qc"]][[k-1]],
                                 sep=",")
  }
  print("writing out...")
  rmTouch("All.ddv")
  writeLines(LOLprint(tempContainer2), "All.ddv")
}

#printME()
printMe2<-function(){
  print("binding")
  chamberId = chamberId[2:length(chamberId)] # ALIGN meta data to data.
  geneName = geneName[2:length(geneName)]
  ctCall = ctCall[2:length(ctCall)]
  peakCall = peakCall[2:length(peakCall)]
  contense <- cbind(chamberId,
                    geneName,
                    ctCall,
                    peakCall,
                    m1Fluo[["F0"]],
                    m1Fluo[["Qc"]], 
                    m5Fluo[['F0']],
                    m5Fluo[['Qc']],
                    m6Fluo[["F0"]],
                    m6Fluo[["Qc"]],
                    m7Fluo[["F0"]],
                    m7Fluo[["Qc"]],
                    m8Fluo[["F0"]],
                    m8Fluo[["Qc"]])
  
  chamberId = rbind( "chamberID",chamberId) ## restor meta data columns to as they were before.
  geneName = rbind("geneName" , geneName)
  ctCall = rbind("ctCall",ctCall)
  peakCall = rbind("peakCall",peakCall)
  
  #header <- list("Chamber ID", "Gene Name", "Ct Call" , "Peak Call","Sigmoidal Initial","Richard's Cy0", "LinRegPCR","LinRegPCR QC", "LRE-Emax", "LRE-Emax QC", "Cy0","Cy0 Qc", "5PSM" ,"5PSM Qc")
  #LongTable <- rbind(header,contense)
  print("Writing")
  rmTouch("All.ddv")
  write.table(contense, file = "All.ddv", append = FALSE, quote = FALSE, sep = ",")
}

