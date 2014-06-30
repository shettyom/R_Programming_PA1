corr <- function(directory, threshold=0){
  cors<-vector()
  c_nobs<-data.frame()
  count<-1
  
  files_full <- list.files(directory, full.names = TRUE)
  n<-NROW(files_full)
  
  c_nobs<-complete(directory)
  
  for (i in 1:n){
    x<-read.csv(files_full[i])
    
    if (c_nobs$nobs[i]>threshold){
      x_cor<-cor(x$sulfate,x$nitrate,use="pairwise.complete.obs")    
      
      
      cors[count]<-x_cor 
      count<-count+1
    }
  }
  print(cors)
}