#Methods file

#input generator functions
makeSelectorFit <- function(n,pval,params,outpath) {
  buffer=c(selector_template,fit_template,atmo_template,prof_template,h1d_template)
  fnames=c("SELECTOR.IN","FIT.IN","ATMOSPH.IN","PROFILE.DAT","HYDRUS1D.DAT")
  values=c(pval)
  for(p in 1:length(params)) {
    for(qq in 1:length(buffer))
      buffer[qq]=gsub(paste("%",toupper(params[p]),"%",sep=""),values[p],buffer[qq])
  }
  for(qq in 1:length(buffer)) {
    writeChar(buffer[qq],paste(outpath,"\\",n,"\\",fnames[qq],sep=""))
  }
}

makeMasterBatch <- function(nsim,sidr) {
  buffer="@echo off\n"
  for(kk in 1:(nsim)) {
    buffer=paste(buffer,"cd .\\",kk,"\n",sep="")  
    buffer=paste(buffer,"call sim.bat\n", sep="")  
    #buffer=paste(buffer,"cd ..\n", sep="")  
  }
  write(buffer,paste(sidr,"master.bat",sep=""))
}

#batch file format
#@echo S:\Andrew\Code\alfalfa_gb_git\Simulations\2\1 > "C:\hydrus1d\Level_01.dir"
#C:
#cd C:\hydrus1d\
#H1D_clci<return.txt
#buffer=paste(buffer,"\"C:\\hydrus1d\\H1D_clci.exe\" .\\",kk,"\n", sep="")  
makeBatch <- function(last_index,n,outpath,simdir) {
  buffer="@echo off\n" 
  wd=gsub("/","\\",getwd(),fixed=TRUE)
  for(kk in 1:(last_index)) {
    buffer=paste(buffer,"@echo ",simdir,n,"\\",kk," > C:\\hydrus1d\\Level_01.dir\nC:\ncd C:\\hydrus1d\\\nH1D_clci<return.txt\n",substr(simdir,0,1),":\ncd ",simdir,"\n", sep="") 
  }                   
  write(buffer,paste(outpath,"\\sim.bat",sep=""))
}

getSeq <- function(range, disc){
  return(seq(range[1],range[2],(range[2]-range[1])/disc))
}

getParams <- function(qq) {
  glib=(gsub("range_","",rownames(qq)))
  return(glib)
}

makeGrid <- function(qq,d) {
  gnames=gsub("range_","d_",rownames(qq))
  buf=list()
  for(p in 1:length(rownames(qq))) {
    lux=qq[p,2:3]
    buf[[p]]=getSeq(as.matrix(lux),d)
  }
  return(expand.grid(buf))
}

#optimization script functions
readNodeFile <- function(id,sim,simdir) {
  fn="OBS_NODE.out"
  fl="Node"
  return(readH1DFile(id,sim,fn,4,list(a="1.234",b="1.234",c="1.234",d="1.234"),12,1,simdir))
}

readFitOutFile <- function(id,sim,simdir,fp) {
    fn="FIT.out"
    fl="Node"
    buf=(readH1DFile(id,sim,fn,7,list(a="1.234",b="1.234",c="1.234",d="1.234",e="1.234",f="1.234",g="1.234"),121,1,simdir))
	return(buf[1:fp,c(2,4)])
}

readFitIn <- function(id,sim,simdir) {
  fn="FIT.IN"
  return(readH1DFile(id,sim,fn,5,list(a="1.234",b="1.234",c="1.234",d="1.234",e="1.234"),16,3,simdir))
}
fee=""
#H1D FILE reader
readH1DFile <- function(id,sim,fname,cols,whatt,skipp,trimm,simdir) {
  obs_node_fname=paste(simdir,sim,"\\",id,"\\",fname,sep="")
  print(paste(sim, id))
  con=file(obs_node_fname, open="r")
  flag=FALSE
  iyx=suppressWarnings(scan(con,what=whatt,skip=skipp,fill=TRUE))
  close(con)
  mat=matrix(unlist(iyx),ncol=cols)
  mat=mat[1:(length(mat[,1])-trimm),]
  class(mat)="numeric"
  return(mat)
}

getFitPoints <- function(x,fit,int=15) {
  x=array(x)
  mmin=0
  mmax=0
  buf=c()
  for(i in 1:length(fit)) {
    mmax=int*i
    interval=(x[((x<=mmax) + (x>mmin))-1])
    buf=c(buf,rep(i,length(interval)))
    mmin=mmax
  }
  return(buf)
}

makeLimit <- function(tplist) {
  xbar=tplist[1,1:length(tplist[1,])]
  inter=2*tplist[2,1:length(tplist[1,])]
  buf=lapply(1:length(xbar),function(i) return(c(xbar[i]-inter[i],xbar[i]+inter[i])))
  #shell.exec(paste("cp .\\LIMITS.IN .\\LIMITS",strftime(Sys.time(),format="%d%m%Y:%H%M%S"),".IN",sep=""))
  buf2=list(params,xbar)
  names(buf)=params
  #write.csv(buf,"LIMITS.IN")
  #write.csv(buf2,"CENTERS.IN")
}