pollutantmean <- function(directory, pollutant, id=1:332){
  files_full <- list.files(directory, full.names = TRUE)
  
  dat<-data.frame()
  for (i in id){
    dat<-rbind(dat, read.csv(files_full[i]))
  }
  if (pollutant=='sulfate'){mean1<-mean(dat$sulfate, na.rm=TRUE)}
  if (pollutant=='nitrate'){mean1<-mean(dat$nitrate, na.rm=TRUE)}
  mean1
}
