#Problems

getwd()
setwd("C:/Users/Acer/Desktop/specdata")  #setting up the desired working directory
getwd()
#1
##creating the function pollutantmean that calculates the mean of a pollutant(either sulfate or nitrate) accross a specified list of monitors.
pollutantmean <- function(directory, pollutant, id = 1: 332){
    myfiles <- list.files(path=directory, pattern=".csv") 
    x <- numeric()
    
    for(i in id){
        mydata <- read.csv(myfiles[i])
        x <- c(x,mydata[[pollutant]])
    }
    
    mean(x, na.rm = TRUE)
}
##examples for item 1
pollutantmean("C:/Users/Acer/Desktop/specdata", "sulfate", 1:10)

pollutantmean("C:/Users/Acer/Desktop/specdata", "nitrate", 70:72)

pollutantmean("C:/Users/Acer/Desktop/specdata", "nitrate", 23)



#2
##creating a function complete that reads a directory full of files and reports the number of completely observed cases in each data.

complete <- function(directory, id = 1:332){
  myfiles <- list.files(path=directory, pattern=".csv")
  nobs <- numeric()
    
    for(i in id){
      mydata <- read.csv(myfiles[i])
      mysum <- sum(complete.cases(mydata))
      nobs <- c(nobs, mysum)
      
    }

  data.frame(id, nobs)
}

##examples for item 2
complete("C:/Users/Acer/Desktop/specdata", 1)

complete("C:/Users/Acer/Desktop/specdata", c(2, 4, 8, 10, 12))

complete("C:/Users/Acer/Desktop/specdata", 30:25)

complete("C:/Users/Acer/Desktop/specdata", 3)

#3
##creating the function corr that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold.


corr <- function(directory, threshold=0){
  myfiles <- list.files(path=directory, pattern=".csv")
  dafr <- complete(directory)
  ids <- dafr[dafr["nobs"] > threshold, ]$id
  corr2 <- numeric()
  
  for(i in ids){
    mydata <- read.csv(myfiles[i])
    dafr2 <- mydata[complete.cases(mydata), ]
    corr2 <- c(corr2, cor(dafr2$sulfate, dafr2$nitrate))
  }
  
  return(corr2)
}

##examples for item 3

cr <- corr("C:/Users/Acer/Desktop/specdata", 150)
head(cr); summary(cr)

cr <- corr("C:/Users/Acer/Desktop/specdata", 400)
head(cr); summary(cr)

cr <- corr("C:/Users/Acer/Desktop/specdata", 5000)
head(cr); summary(cr); length(cr)

cr <- corr("C:/Users/Acer/Desktop/specdata")
head(cr); summary(cr); length

##4
setwd("C:/Users/Acer/Desktop/file 2")

# Reading in data

outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])

hist(outcome[, 11],
     main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack",
     xlab = "Deaths",
     col="lightblue"
     )

