library(rgl)
#setwd("~/Projects/Craig")

haim <- function(gD){
  # Haim's code reuires preprocessed Rox and probe data.
  # To get this we will construct 
  print('**')
  k <- 1
  first = T
  Rox <- list()
  Probe <- list()
  while(k<=length(gD[[names(gD)[[1]]]][[names(gD[[ names(gD)[[1]] ]])[[2]]]])){#first file probe background. (this should line up with the fourground data...)
    #print('*_*')
    tempR <- list()
    tempP <- list()
    k2 <- 1
    if(first){
      first =F
      #tempP<-gD[[ names(gD)[[1]] ]] [[names(gD[[ names(gD)[[1]] ]])[[2]] ]] [[1]]
      #tempR<-gD[[ names(gD)[[1]] ]] [[names(gD[[ names(gD)[[1]] ]])[[3]] ]] [[1]]
      First<-T
      while(k2<=length(gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]]]])[[2]] ]] [[k]])){
        if(First){
          First<-F
          tempP[[k2]]<-paste(gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[2]] ]] [[k]][[k2]],gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[4]] ]] [[k]][[k2+1]],sep="-",collapse=NULL)#"Bkgd Data for Probe EvaGreen"
          tempR[[k2]]<-paste(gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[3]] ]] [[k]][[k2]],gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[5]] ]] [[k]][[k2+1]],sep="-",collapse=NULL)#"Bkgd Data for Passive Reference ROX"
          k2 <- k2 + 2    # ^ gets the first 2 colums and colapses them into one column.
          #tempP[[k2]]<-gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[4]] ]] [[k]][[k2]]#"Raw Data for Probe EvaGreen"
          #tempR[[k2]]<-gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[5]] ]] [[k]][[k2]]#"Raw Data for Passive Reference ROX"
          #k2 <- k2 + 1
        }else{
          tempP[[k2-1]] <- as.numeric(gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[4]] ]] [[k]][[k2]])
          #Subtract backrounds from Raw
          tempR[[k2-1]] <- as.numeric(gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[5]] ]] [[k]][[k2]])
          k2 <- k2+1
        }
      }
    }else{
      First<-T
      while(k2<=length(gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]]]])[[2]] ]] [[k]])){
        if(First){
          First<-F
          tempP[[k2]]<-paste(gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[2]] ]] [[k]][[k2]],gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[4]] ]] [[k]][[k2+1]],sep="-",collapse=NULL)#"Bkgd Data for Probe EvaGreen"
          tempR[[k2]]<-paste(gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[3]] ]] [[k]][[k2]],gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[5]] ]] [[k]][[k2+1]],sep="-",collapse=NULL)#"Bkgd Data for Passive Reference ROX"
          k2 <- k2 + 2    # ^ gets the first 2 colums and colapses them into one column.
          #tempP[[k2]]<-gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[4]] ]] [[k]][[k2]]#"Raw Data for Probe EvaGreen"
          #tempR[[k2]]<-gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[5]] ]] [[k]][[k2]]#"Raw Data for Passive Reference ROX"
          #k2 <- k2 + 1
        }else{
          tempP[[k2-1]] <- as.numeric(gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[4]] ]] [[k]][[k2]]) - as.numeric(gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[2]] ]] [[k]][[k2]])
          #Subtract backrounds from Raw
          tempR[[k2-1]] <- as.numeric(gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[5]] ]] [[k]][[k2]]) - as.numeric(gD[[ names(gD)[[1]] ]] [[ names(gD[[ names(gD)[[1]] ]])[[3]] ]] [[k]][[k2]])
          k2 <- k2+1
        }
      }
    }
    #print('*AppendTempsto data*')
    Rox[[k]]<-tempR
    Probe[[k]]<-tempP
    k <- k+1
  }
  system(paste("echo "))
  writeLines(LOLprint(Rox),"Rox.ddv")
  writeLines(LOLprint(Probe),"Probe.ddv")
  writeLines(LOLprint(gD[[ names(gD)[[1]] ]] [['expInfo']]),"Keys.ddv")
  return(list(Rox,Probe))
}

hs <- function(Rox,Probe,keyFile){
  cat("Haim ...\n")
  cat("... Reading Rox")
  RawROX = read.csv(Rox,header=TRUE)         #PreProcessed data
  cat("\r")
  cat("... Reading Probe")
  RawFAMMGB = read.csv(Probe,header=TRUE)
  RawROX = RawROX[order(RawROX[,1]),]
  RawFAMMGB = RawFAMMGB[order(RawFAMMGB[,1]),]
  # 27648 chamber IDs = 96*(NROW(keys)/96)
  
  Rn = RawFAMMGB[,2:NCOL(RawFAMMGB)]/RawROX[,2:NCOL(RawFAMMGB)]
  # get the reference level for t=35 using the Negative samples, across all genes:
  
  cat("\r")
  cat("... Reading Keys")
  
  keys = read.csv(keyFile ,header=TRUE)
  
  cat("\r")
  cat("...hs Input Complete...\n")
  keys = keys[order(keys[,1]),]
  genes = keys[,5]
  # (NROW(keys)/96) levels of "keys"
  cells = gsub(" \\d+$","",levels(keys[,2]),perl=T)
  trunckeys = gsub(" \\d+$","",keys[,2],perl=T)
  id="Negative"
  grp = which(trunckeys == id)
  grpkeys = keys[grp,1]
  rws = (RawROX$Chamber.ID%in%grpkeys)
  rn = Rn[rws,]
  ref1 = mean(rn[,1])
  ref35 = mean(rn[,NCOL(RawFAMMGB)-1])
  deltaref = ref35-ref1
  
  cat("")

  # create a table of 96 genes X (NROW(keys)/96) cells
  ratio = (Rn[,NCOL(RawFAMMGB)-1]/Rn[,1])-(rn[,NCOL(RawFAMMGB)-1]/rn[,1])
  ratiomat = matrix(ratio,nrow=96,ncol=(NROW(keys)/96))
  colnames(ratiomat) = trunckeys[seq(1,length(trunckeys),by=96)]
  hist(ratiomat,breaks=60)
  # looks like a mixture, when looking at all cells, across all genes.
  # define a distance based on the mixture: for each gene/cell find 
  # p=Pr(in right component)
  # Then, for each cell we have a 96-component vector and we can compute
  # the distance between cells (e.g. KL, max, euc), and perform clustering/PCA
  #
  
  cols = rep(1,(NROW(keys)/96))
  pchs = rep(1,(NROW(keys)/96))
  tdtxt = rep("a",(NROW(keys)/96))
  rnb = rainbow(8)

  cat("skip1")
  Reprogramming = F
  if(Reprogramming){
    pchs[grep("Negative",colnames(ratiomat))] = 1
    pchs[grep("Control",colnames(ratiomat))] = 16
    pchs[grep("BJ",colnames(ratiomat))] = 8
    pchs[grep("H1",colnames(ratiomat))] = 3
    pchs[grep("H9",colnames(ratiomat))] = 3
    pchs[grep("SSEA4\\- Tra1-60\\-",colnames(ratiomat))] = 25
    pchs[grep("SSEA4\\+ Tra1-60\\-",colnames(ratiomat))] = 24
    pchs[grep("SSEA4\\+ Tra1-60\\+",colnames(ratiomat))] = 22
    
    cols[grep("Negative",colnames(ratiomat))] = "gray33"
    cols[grep("Control",colnames(ratiomat))] = "gray33"
    cols[grep("BJ",colnames(ratiomat))] = rnb[1]
    cols[grep("H1",colnames(ratiomat))] = rnb[7]
    cols[grep("H9",colnames(ratiomat))] = rnb[8]
    cols[grep("D4",colnames(ratiomat))] = rnb[2]
    cols[grep("D7",colnames(ratiomat))] = rnb[3]
    cols[grep("D11",colnames(ratiomat))] = rnb[4]
    cols[grep("D14",colnames(ratiomat))] = rnb[5]
    cols[grep("D21",colnames(ratiomat))] = rnb[6]
    
    tdtxt[grep("Negative",colnames(ratiomat))] = "N"
    tdtxt[grep("Control",colnames(ratiomat))] = "C"
    tdtxt[grep("BJ",colnames(ratiomat))] = "BJ"
    tdtxt[grep("H1",colnames(ratiomat))] = "H1"
    tdtxt[grep("H9",colnames(ratiomat))] = "H9"
    tdtxt[grep("SSEA4\\- Tra1-60\\-",colnames(ratiomat))] = "mm"
    tdtxt[grep("SSEA4\\+ Tra1-60\\-",colnames(ratiomat))] = "pm"
    tdtxt[grep("SSEA4\\+ Tra1-60\\+",colnames(ratiomat))] = "pp"
    
    lgd = c("Negative","Control","BJ","H1","H9",
            "S-T- D4","S-T- D7","S-T- D11","S-T- D14","S-T- D21", 
            "S+T- D4","S+T- D7","S+T- D11","S+T- D14","S+T- D21",
            "S+T+ D7","S+T+ D11","S+T+ D14","S+T+ D21")
  }
  cat(",2")
  lgdcol = c("gray33","gray33", rnb[1],rnb[7],rnb[8],
             rnb[2],rnb[3],rnb[4],rnb[5],rnb[6],
             rnb[2],rnb[3],rnb[4],rnb[5],rnb[6],
             rnb[3],rnb[4],rnb[5],rnb[6])
  lgdpch = c(1,16,8,3,3,25,25,25,25,25,24,24,24,24,24,22,22,22,22)
  
  X = t(ratiomat)
  pca = prcomp(X,center=TRUE)
  pca$sdev^2/sum(pca$sdev^2)
  cumsum(pca$sdev^2/sum(pca$sdev^2))
  plot(pca$sdev^2/sum(pca$sdev^2)) # 2, maybe 3 components. Even 4 may be OK
  
  cat(",3")
  if(Reprogramming){
    pdf("file1.pdf",onefile=T)
    plot(pca$x[,1],pca$x[,2],col=cols,pch=pchs,xlim=c(-2.5,5),
         xlab="PC1",ylab="PC2",asp=1)
    legend(3.2,3.4,legend=lgd,cex=0.75,bty="n",text.col=lgdcol,pch=lgdpch,col=lgdcol)
    
    plot(pca$x[,1],pca$x[,3],col=cols,xlim=c(-2.5,5),asp=1,
         pch=pchs,xlab="PC1",ylab="PC3")
    legend(3.2,3.4,legend=lgd,cex=0.75,bty="n",text.col=lgdcol,pch=lgdpch,col=lgdcol)
    
    plot(pca$x[,2],pca$x[,3],col=cols,xlim=c(-2.5,5),asp=1,
         pch=pchs,xlab="PC2",ylab="PC3")
    legend(3.2,3.4,legend=lgd,cex=0.75,bty="n",text.col=lgdcol,pch=lgdpch,col=lgdcol)
    dev.off()
    
    r3dDefaults$windowRect <- c(0,50, 900, 900)
    plot3d(pca$x[,1],pca$x[,2],pca$x[,3],col=cols, size=2,xlim=c(-3,3),ylim=c(-3,3),zlim=c(-3,3))
    text3d(pca$x[,1],pca$x[,2],pca$x[,3],tdtxt,col=cols)
    save(pca,cols,tdtxt,file="pca.RData")
  
    pca$x[,1:3]
    
    D=dist(t(ratiomat))
    D = dist(pca$x)
    fit <- isoMDS(D)
    fit$stress #11.29 
    plot(fit$points)
    
    fit <- isoMDS(D,k=3)
    fit$stress # 8.55
    r3dDefaults$windowRect <- c(0,50, 900, 900)
    plot3d(fit$points[,1],fit$points[,2],fit$points[,3],col=cols, size=2)
    text3d(fit$points[,1],fit$points[,2],fit$points[,3],tdtxt,col=cols)
    
    fit <- isoMDS(D,k=4) # 6.97%
    
    # k=6 => 5.09%
    
    
    # add PC4
    
    plot(pca$rotation[,1],pca$rotation[,4],col=cols,xlim=c(-0.01,0.15),ylim=c(-0.2,0.2),pch=pchs,
         xlab="PC1",ylab="PC4")
    legend(0.11,0.2,legend=lgd,cex=0.75,bty="n",text.col=lgdcol,pch=lgdpch,col=lgdcol)
    
    plot(pca$rotation[,2],pca$rotation[,4],col=cols,xlim=c(-0.15,0.15),ylim=c(-0.2,0.2),pch=pchs,
         xlab="PC2",ylab="PC4")
    legend(0.11,0.2,legend=lgd,cex=0.75,bty="n",text.col=lgdcol,pch=lgdpch,col=lgdcol)
    
    plot(pca$rotation[,3],pca$rotation[,4],col=cols,xlim=c(-0.15,0.2),ylim=c(-0.2,0.2),pch=pchs,
         xlab="PC3",ylab="PC4")
    legend(0.15,0.2,legend=lgd,cex=0.75,bty="n",text.col=lgdcol,pch=lgdpch,col=lgdcol)
    
    #scatter3D(pca$rotation[,1],pca$rotation[,2],pca$rotation[,3],pch=pchs,col=cols,colkey=F,
    #          phi = 30, theta = 60)
    
    plot(pca$rotation[,1],pca$rotation[,3],col="white",xlim=c(-0.01,0.15),ylim=c(-0.2,0.2))
    text(pca$rotation[,1],pca$rotation[,3],cols,col=cols,cex=0.6)
    legend(0.12,0.2,legend=paste(1:20,unique(cells)),cex=0.45,bty="n",text.col=cols)
    
    plot(pca$rotation[,1],pca$rotation[,4],col="white",xlim=c(-0.01,0.15),ylim=c(-0.2,0.2))
    text(pca$rotation[,1],pca$rotation[,4],cols,col=cols,cex=0.6)
    legend(0.12,0.2,legend=paste(1:20,unique(cells)),cex=0.45,bty="n",text.col=cols)
    
    
    plot(pca$rotation[,2],pca$rotation[,3],col="white",xlim=c(-0.1,0.2),ylim=c(-0.2,0.2))
    text(pca$rotation[,2],pca$rotation[,3],cols,col=cols,cex=0.6)
    legend(0.12,0.2,legend=paste(1:20,unique(cells)),cex=0.45,bty="n",text.col=cols)
    
    plot(pca$rotation[,2],pca$rotation[,4],col="white",xlim=c(-0.01,0.15),ylim=c(-0.2,0.2))
    text(pca$rotation[,2],pca$rotation[,4],cols,col=cols,cex=0.6)
    legend(0.12,0.2,legend=paste(1:20,unique(cells)),cex=0.45,bty="n",text.col=cols)
    
    plot(pca$rotation[,3],pca$rotation[,4],col="white",xlim=c(-0.01,0.15),ylim=c(-0.2,0.2))
    text(pca$rotation[,3],pca$rotation[,4],cols,col=cols,cex=0.6)
    legend(0.12,0.2,legend=paste(1:20,unique(cells)),cex=0.45,bty="n",text.col=cols)
  }
  
  cat("\nHeat Map")
  # # # # # # # # # # #
  
  heatmap(ratiomat)
  ratiomat2 = ratiomat
  colnames(ratiomat2) = tdtxt
  D = dist(t(ratiomat2))
  hc= hclust(D)
  plot(hc,cex=0.5) 
  cat("Heatmap Complete\n")
  # # # # # # # # # # # Clustering and plot.
  
  cat("Histo processing\n")
  
  lims = c(min(Rn),max(Rn))
  
  cnt = 0
  cat("id in unique(cells) -- 1")
  for( id in unique(cells)) { #"D4 SSEA4+ Tra1-60- 2"
    cnt =cnt+1
    cat(cnt, id,"\n")
    grp = which(trunckeys == id)
    grpkeys = keys[grp,1]
    rws = which(RawROX$Chamber.ID%in%grpkeys)
    #  png(filename=sprintf("%s.png",id),width=5,height=5,units="in",res=150)
    plot(t(Rn[rws[1],]),ylim=lims,cex=0.5,main=id,type='l',col="gray")
    for (i in 2:length(rws)) {
      lines(t(Rn[rws[i],]),col="gray")
    }
    #  dev.off()
  }
  #cat("\r")
  # skip
  cnt = 0
  #<< -- >> Raw Fluorescence Delta histos << -- >>
  png(filename="hists.png",width=8,height=11,units="in",res=150)
  par(mfrow=c(6,4))
  for( id in unique(cells)) { #"D4 SSEA4+ Tra1-60- 2"
    cnt =cnt+1
    cat(cnt, id,"\n")
    grp = which(trunckeys == id)
    grpkeys = keys[grp,1]
    rws = which(RawROX$Chamber.ID%in%grpkeys)
    hist(log(Rn[rws,NCOL(RawFAMMGB)-1]/Rn[rws,1]),xlim=c(-0.1,1),main=id)
  }
  dev.off()
  
  
  cnt = 0
  cat("id in unique(cells) -- 2")
  for( id in unique(cells)) {
    cnt =cnt+1
    cat(cnt, id,"\n")
    grp = which(trunckeys == id)
    grpkeys = keys[grp,1]
    # id=BJ: Tet1 - no effect, GAPDH - clear effect, FOXD1 - mixed
    rws = intersect(which(RawROX$Chamber.ID%in%grpkeys),which(genes == "FOXD1"))
    rn = Rn[rws,] - mean(Rn[rws,1])
    #  png(filename=sprintf("%s.png",id),width=5,height=5,units="in",res=150)
    plot(t(rn[1,]),ylim=c(-1,2),cex=0.5,main=id,type='l',col="gray")
    for (i in 2:length(rws)) {
      lines(t(rn[i,]),col="gray")
    }
    hist(rn[,NCOL(RawFAMMGB)-1] - deltaref)
    t.test(rn[,NCOL(RawFAMMGB)-1] - deltaref)
    #  dev.off()
  }
  #cat("\r")
  
  
  # cnt = 0
  # for( id in levels(keys[,2])) { #"D4 SSEA4+ Tra1-60- 2"
  #   cnt =cnt+1
  #   cat(cnt, id,"\n")
  #   grp = which(keys[,2] == id)
  #   grpkeys = keys[grp,1]
  #   rws = which(RawROX$Chamber.ID%in%grpkeys)
  #   plot(t(Rn[rws[1],]),ylim=lims,cex=0.5,main=id,type='l')
  #   for (i in 2:length(rws)) {
  #     lines(t(Rn[rws[i],]),cex=0.5)
  #   }
  # }
  
}

