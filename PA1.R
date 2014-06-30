rm(list=ls())

setwd("C:\\ORS\\R Training\\Coursera\\R Programming\\PA1")
#list.files("specdata")

#Saving the names of all files in specdata in character variable files_full
files_full <- list.files("specdata", full.names = TRUE)

#total number of files in folder
n<-NROW(files_full)

n<-3:50

#Combine all csv files into a single data frame
dat<-data.frame()
for (i in n){
  dat<-rbind(dat, read.csv(files_full[i]))
}

x<-"nitrate"
if (x=='sulfate'){mean1<-mean(dat$sulfate, na.rm=TRUE)}
if (x=='nitrate'){mean1<-mean(dat$nitrate, na.rm=TRUE)}

## Assembling the Pollutantmean function ##

setwd("C:\\ORS\\R Training\\Coursera\\R Programming\\PA1")

pollutantmean <- function(directory, pollutant, id){
  files_full <- list.files(directory, full.names = TRUE)
  
dat<-data.frame()
  for (i in id){
    dat<-rbind(dat, read.csv(files_full[i]))
  }
  x<-"nitrate"
  if (pollutant=='sulfate'){mean1<-mean(dat$sulfate, na.rm=TRUE)}
  if (pollutant=='nitrate'){mean1<-mean(dat$nitrate, na.rm=TRUE)}
  mean1
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

# agg1<-aggregate(dat, by=list(dat$id), mean)
# 
# aggregate(dat, list(id="1"), mean)

# library(plyr)
# ddply(dat, .(id), summarize, nitrate = count(nitrate),na.rm=true)

df1 <- aggregate(cbind(sulfate,nitrate)~ID,data=dat,  FUN = count(complete.cases))
head(df1)

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

c_nobs<-data.frame()
c_nobs<-complete("specdata")


i<-1
if (c_nobs[i,2]>100){
  x_cor<-cor(x$sulfate,x$nitrate)    
}
cor(x$sulfate,x$nitrate,use="pairwise.complete.obs")