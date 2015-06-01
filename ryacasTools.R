#############################################
#Package is liscensed by                    #
#  KrewnSolutions   /< /? [- \/\/ |\|       #
#http://dev.perl.org/licenses/artistic.html #
#liscense holder: kpie314(a)gmail.com       #
#############################################
library(Ryacas)
x<-Sym("x")

derivToFunc<-function(d){
  a <- function(X){ 
    return(eval(d,list(x=X) ) )}
  return(a)}

retToVal<-function(q){return(as.numeric(attributes(q)$gradient))}

derive <- function(expr,x){
  f <- deriv(expr,x)    #FromPackage Ryacas
  #print(f)
  a <- derivToFunc(f$grad$x)   #f is an object of the class created by Ryacas
  #print(a)
  r <- function(X){return(retToVal(a(X)))} #r Will now return the slope of
  #print(r)
  s <- strsplit(as.character(f),"\n")
  return(r)
}

symFromDeriv <- function(deri){
  #print(deri)
  s <- strsplit(as.character(deri),"\n")
  #print("greppy")
  #print(s)
  expressions <- list()
  temp <- grepl("    .expr",s[[1]])
  #print(temp)
  #print("*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*")
  for(k in c(1:length(s[[1]]))){
    if(temp[k]){
      temp2 <- strsplit(s[[1]][k],"<-")
      #temp2[[1]][1] <- paste(".",strsplit(s[[1]][k],".")[[1]][2],sep="",collapse=NULL)
      #print(gsub("    ","",temp2[[1]][1]))
      expressions[[ gsub(" ","",temp2[[1]][1]) ]] <- paste("(",temp2[[1]][2],")", sep = "")
    }
  }
  for( k in names(expressions)) {
   # print(k)
  #  print(expressions[[k]])
  #  print(lapply(unlist(names(expressions)), function(x) grep(x,expressions[[k]] ) ))
  #  print(paste("sum ",k,":",sum(unlist(lapply(unlist(names(expressions)), function(x) grep(gsub(" ","",x),expressions[[k]] ) )))))
    while(sum(unlist(lapply(unlist(names(expressions)), function(x) grep(x,expressions[[k]] ) )))>0){
      for(k2 in names(expressions)){
        #print(grep(k2,expressions[[k]]))
        if(length(grep(k2,expressions[[k]]))>0){
          #print(paste("k:",k,sep="",collapse=NULL))
          #print(paste("k2:",k2,sep="",collapse=NULL))
          #print(expressions[[k]])
          a<-gsub(k2,expressions[[k2]],expressions[[k]])
          #print(typeof(gsub(k2,expressions[[k2]],expressions[[k]])))
          #print(typeof(expressions[[k]]))
          expressions[[k]] <- a
        }
      }
    }
  }
 # print("$_____________________________________________________________________$")
  #print(names(expressions))
  #print(expressions)
  #print("grad")
  D <- grepl("    .grad\\[",s[[1]])
  #print(grepl("    .grad\\[",s[[1]]))
  q<-0
  for(k in c(1:length(s[[1]]))){
    #print(D[[k]])
    #print(s[[1]][k])
    if(D[[k]]){
      #print("swish")
      q <- k
    }}
  #print(s[[1]][q])
  d <- strsplit(s[[1]][q],"<-")[[1]][2]
  #print(d)
  #print(D)
  #print("&!!!!!!!!!!!!!!!!!!!!!&")
  for(k in names(expressions)){
  #  print(paste("d:",d))
  #  print(paste("k:",k))
  #  print(expressions[[k]])
  #  print("--")
  #  print(gsub(gsub(" ","",k),expressions[[k]],d))
    d <- gsub(gsub(" ","",k),expressions[[k]],d)
  #  print("-")
  }
  #print(d)
  #print("&!!!!!!!!!!!!!!!!!!!!!&")
  #return(eval(parse(text = paste("~",strsplit(s[[1]][4],"<-")[[1]][2]))))
  #print(d)
  return(d)
}

getVars<-function(str){
  val <- as.raw(list(88,89,90)) # use lower case and upper case x,y,z,X,Y,Z (cap sensitive)
  vars = list()
  c = 1
  for(k in val){        # Search the alphabet for variable Names
    if(grepl(rawToChar(k),str)){
      #print(paste(rawToChar(as.raw(as.numeric(k)+32)),str))
      vars[c]=rawToChar(k)
      c<-c+1
    }
    if(grepl(rawToChar(as.raw(as.numeric(k)+32)),str)){
      #print(paste(rawToChar(as.raw(as.numeric(k)+32)),str))
      vars[c]=rawToChar(as.raw(as.numeric(k)+32))
      c<-c+1
    }
    #if(grepl(rawToChar(k),str)){
    #  if(length(vars)==0){vars<-rawToChar(k)}
    #  else{vars<-paste(vars,rawToChar(k),sep = ",",collapse = NULL)}
    #}
    #if(grepl(rawToChar(as.raw(as.numeric(k)+32)),str)){ #Lower case are considered to be seperate varibales.
    #  if(length(vars)==0){vars<-rawToChar(as.raw(as.numeric(k)+32))}
    #  else{vars<-paste(vars,rawToChar(as.raw(as.numeric(k)+32)),sep = ",",collapse = NULL)}
    #}
  }
  return(vars)
}

makeSym <- function(str){
  val <- sapply(c(88,89,90),as.raw) # use lower case and upper case x,y,z,X,Y,Z (cap sensitive)
  for(k in val){        # Search the alphabet for variable Names
    #print(paste("Loop:",rawToChar(k),":",rawToChar(as.raw(as.numeric(k)+32)),sep="",collapse=NULL))
    if(grepl(rawToChar(k),str)){
      eval(parse(text=paste(rawToChar(k),"<-Sym('",rawToChar(k),"')",sep="" )))
    }
    if(grepl(rawToChar(as.raw(as.numeric(k)+32)),str)){ #Lower case are considered to be seperate varibales.
      eval(parse(text=paste(rawToChar(as.raw(as.numeric(k)+32)),"<-Sym('",rawToChar(as.raw(as.numeric(k)+32)),"')",sep="" )))
    }
  }
  x<-eval(parse(text=paste("~",str,sep="",collapse=NULL)))
  return(x)
}

listPrint<-function(lst){
  First = T
  ret = ""
  for(k in lst){
    if(First){
      ret = k
      First = F
    }else{
      ret = paste(ret,k,sep = ",",collapse=NULL)
    }
  }
  return(ret)
}

makeFunc <- function(str){# Variables are added to the function in alphabetical order.
  vars <- getVars(str)
  if(length(vars)==0){
    vars = list("x")
  }
  x<-eval(parse(text=paste("function(",listPrint(vars),"){return(",str,")}",spe="",collapse=NULL)))
  return(Vectorize(x))
}

dydx<-function(cf){
  #print('_____________________________________________________________________________')
  #print(cf$sym)
  #print(deriv(cf$sym,x))
  #print(paste("symFromDeriv(deriv(cf$sym,x):",symFromDeriv(deriv(cf$sym,x) )))
  #print(as.character(symFromDeriv(deriv(cf$sym,x) )))
  #print(gsub("~","",as.character(symFromDeriv(deriv(cf$sym,x) ))))
  #print('_____________________________________________________________________________')
  return(calcFunc(gsub("~","",as.character(symFromDeriv(deriv(cf$sym,x) )))))
}

#slove<-function(cf,n){
#  
#}

ksolve <- function(y,func){
  print(y)
  print(func$sym)
  print(paste("Solve(",func$sym2,"==",y,"x)"))
  q <- yacas(paste("Solve(",func$sym2,"==",y,"x)"))
  q <- gsub("{","",q)
  q <- gsub("}","",q)
  q <- gsub("x==","r =",q)
  #print(q)
  eval(parse(text=q))
  return(r)
}

calcFunc<-function(Str){#http://www.cyclismo.org/tutorial/R/s3Classes.html < OOdesign in R
  #str = Str
  #print(Str)
  #sym = makeSym(str)
  #sym2= gsub("~","",sym)
  #f   <- makeFunc(str)
  #print(listPrint(getVars(str)))
  #der<-function(){return(calcFunc(gsub("~","",as.character(symFromDeriv(deriv(sym,eval(parse(text=as.character(getVars(str))))) )) ) ))}
  #print("wtf2")
  specs <- list(        # Attributes are stored in a list.
    str = Str,
    sym = makeSym(Str),
    sym2= gsub("~","",makeSym(Str)),
    f   = makeFunc(Str)
  )
  #print(as.character(symFromDeriv(deriv (sym,eval (parse (text=getVars(str)))))))
  #print('=============================================================================')
  #print(specs)
  #print('=============================================================================')
  class(specs) <- append(class(specs),specs)
  print("really?")
  return(specs)
}
#s<-calcFunc('tan(3*cos(x^2))')
#c <- dydx(s)
#print("durp")
#curve(s$f(x),-5,5,xname="x")
#curve(c$f(x),-5,5,xname="x")