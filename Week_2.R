library(tidyverse)

#part1
pollutantmean <- function(directory, pollutant, id=1:332){
  files = list.files(directory, full.names = T)
  x <- c()
  for(i in id){
      df <- read.csv(files[i])
      pol <-df[,pollutant][is.na(df[,pollutant])==F]
      x <- append(x,pol)}
  mean(x)
}

#test
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23) 

#part2
complete <- function(directory, id=1:332){
  files = list.files(directory, full.names = T)
  dc <-data.frame(matrix(ncol = 2, nrow=0))
  
  for(i in id){
    da <- read.csv(files[i])
    da <- da[complete.cases(da), ]
    db <- c(i, nrow(da))
    dc <- rbind(dc,db)}
  
  colnames(dc) <- c("id", "nobs")
  dc
}

#test  
complete("specdata", 30:25)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 1)
complete("specdata", 3)
dd <-complete("specdata")

#part3
corr <- function(directory, threshold =0){
  dd <- complete(directory)
  ind <- dd[dd$nobs>threshold,]$id
  x <-c()
  for(i in ind){
    dx <-read.csv(files[i])
    cor <- cor(dx$sulfate, dx$nitrate,use='complete.obs')
    x <- append(x, cor)}
  x
}

#test
cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

ls(environment(corr))
get("files", environment(corr))

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

#quiz 1
pollutantmean("specdata", "sulfate", 1:10)
#4.064

#quiz 2
pollutantmean("specdata", "nitrate", 70:72)
#1.706

#quiz 3
pollutantmean("specdata", "sulfate", 34)
#1.477

#quiz 4
pollutantmean("specdata", "nitrate")
#1.703

#quiz 5
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
#228 148 124 165 104 460 232

#quiz 6
cc <- complete("specdata", 54)
print(cc$nobs)
#219

#quiz 7
RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
#711 135  74 445 178  73  49   0 687 237

#quiz 8
cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)
#0.2688  0.1127 -0.0085  0.4586  0.0447

#quiz 9
cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)
#243.0000   0.2540   0.0504  -0.1462  -0.1680   0.5969

#quiz 10
cr <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
#0.0000 -0.0190  0.0419  0.1901
###
