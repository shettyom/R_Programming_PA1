complete <- function(directory, id = 1:332){
  
  dat<-data.frame()
  x<-data.frame()
  count<-1
  
  id2<-vector()
  nobs<-vector()
  
  for (i in id){
    dat<-rbind(dat, read.csv(files_full[i]))
    x<-read.csv(files_full[i])
    c_nobs<-sum(complete.cases(x))
    id2[count]<-i
    nobs[count]<-c_nobs
    count<-count+1
  }
  dat2<-data.frame(cbind(id2,nobs))
  colnames(dat2) <- c("id","nobs")
  print(dat2)
}