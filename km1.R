source("./ryacasTools.R")
source("./qpcRsources.R")
library(Ryacas)
kcy0 <- function(probe,cycles,p=F){
  Fmax = max(df[[k]])
  fb = mean(probe[1:5])
  c<-which.max(diff(probe))
  # or Fmax = 2*probe[c]-fb
  e = exp(1)
  #print("oh")
#   for(k in 1:length(probe)){
#     if(probe[[k]]>Fmax/2){
#       c = (k-1+(Fmax/2-probe[[k-1]])/(probe[[k]]-probe[[k-1]]))
#       print("!!!!")
#       print(c)
#       break
#     }
#   }
  c<-which.max(diff(probe))
  b<-kscf(probe,cycles,getb=T)
  if(b==0){
    return(0)
  }
  tcBlock = tryCatch({
    fit <<- nls(probe ~ Fmax/(1 + e^((-1/b)*(cycles-c)))^d+fb , start = list(d=0),#5 pram fit with Richard's coeficent on the denominator
              control = list(maxiter = 50, tol = 1e-05, minFactor = 1/1024,printEval = F, warnOnly = T))
    td  <<- F
  }, warning = function(w) {
    print(w)
  }, error = function(e) {
    print(e)
    td <<- T
  }, finally = {
    print(paste("Fitsuccess :",!td))
  }
  )
  #print("yeah")
  d <- coefficients(fit)[["d"]]
  print(d)
  cy0 <- c+b*log(d)-b*(d+1/d)*(1-(fb/Fmax)*(d+1/d)^d)
#   firstD = D(expression(Fmax/(1 + e^((-1/b)*(x-c)))),'x')
#   print(firstD)
#   secdD = D(firstD,'x')
#   p = uniroot(function(x)eval(secdD),c(0,10000))$root
#   print(">>>>>>>>>>>=")
#   print(p)
#   print(">>>>>>>>>>>-")
#   f0 = Fmax/(1+e^(coefficients(fit)[["c"]]/coefficients(fit)[["b"]]))
  print(paste("cy0 > >>>>>",cy0))
  return(cy0)
}

kscf <- function(probe,cycles,p=F,getb=F){
  # df[[k]] will be the cell assay pair fluorescence vector.
  # we have cycles already
  # for given cycle x fluo  fx =  Fmax/(1 + e^((-1/b)*(x-c)) ) + Fb
  Fmax = max(probe)
  e = exp(1)
  print(typeof(probe))
  #print(probe)
  print(probe[1:5])
  fb = mean(probe[1:5])
  #print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  #print(fb)
  td <<- F
  #fit = nls(probe ~ Fmax/(1 + e^((-1/b)*(cycles-c))+fb ) , start = list(b=1 , c = 1 ))
  inflec = which.max(diff(probe))
  c=1
  halfMax = Fmax/2
  while(probe[[c]]<halfMax){
    print(c)
    c = c + 1
  }
  #print("wtf OG")
  if(c>1){
    c = c-1
    c = c + (halfMax-probe[[c]])/(probe[[c+1]]-probe[[c]])
    print(c)
  }else{
    print("fail : c0 > fMax/2")
    return(0)
  }
  tcblock = tryCatch({
      fit <<- nls(probe ~ Fmax/(1 + e^((-1/b)*(cycles-c)) )+fb , start = list(b=probe[inflec+1]-probe[inflec]),
                  control = list(maxiter = 50, tol = 1e-05, minFactor = 1/1024 ,printEval = F, warnOnly = T))
      td  <<- F
    }, warning = function(w) {
      print(w)
    }, error = function(e) {
      print(e)
      td <<- T
    }, finally = {
      print(paste("Fitsuccess :",!td))
    }
  )
  if(td){
    return(0)
    print("phyce")
  }
  if(getb){
    return(coefficients(fit)[["b"]])
  }
  #fit = nls(probe ~ Fmax/(1 + e^((-1/b)*(cycles-c))+fb ) , start = list(b=1 , c = 1 ))
  if(p){
    plot(cycles,probe)
    n = data.frame(x = seq(min(cycles),max(cycles),len=max(cycles)) )
    q = predict(fit,n$x)
    lines(n$x,q)
    #print("wudUP")
  }
  f0 = Fmax/(1+e^(c/coefficients(fit)[["b"]]))
  #print(paste(">>>>>>",f0))
  return(f0)
}

m1 <- function(probe,verbose = F, cy0 = F){# data.frame()
  scflist <- list()
  cy0list <- list()
  print(df[[1]])
  cycles <- df[[1]]
  print(paste(max(cycles)," qpcrCycles", sep = ""))
  for(k in 2:length(probe)) {
    if(verbose){
      print(paste("m1"+k,"  /  ",length(probe),sep = ""))
    }
    scflist[[k]] = kscf(probe[[k]],cycles,p=T)
    cy0list[[k]] = kcy0(probe[[k]],cycles,p=T)
  }
  #print("woot")
  if(cy0){
    return(cy0list))
  }else{
    return(scflist)
  }
}