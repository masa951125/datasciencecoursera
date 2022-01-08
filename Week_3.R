library(tidyverse)

s <- split(airquality, airquality$Month)
sapply(s, function(x) colMeans(x[,c("Ozone", "Solar.R", "Wind")], na.rm = T))

x <- rnorm(10)
f1 <-gl(2,5)
f2 <-gl(5,2)
interaction(f1,f2)

split(x, list(f1,f2), drop = T)
?invisible
f1 <- function(x) x
f2 <- function(x) invisible(x)
f1(1)  # prints
f2(1)  # does not

printmessage <- function(x){
  if(class(x)!="numeric" | is.na(x))
    print("we can not produce result")
  else 
    if(x>=0)
    print("x is positive or 0")
    if(x<0)
    print("x is negative")
  invisible(x)
}

printmessage(0)

#quiz_1
data(iris)
tapply(iris$Sepal.Length, iris$Species, mean, simplify = T)
#6.588

#quiz_2
apply(iris[,1:4], 2, mean)
#Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#5.843333     3.057333     3.758000     1.199333 

#quiz_3
data(mtcars)
head(mtcars)
dim(mtcars)
#the average miles per gallon (mpg) by number of cylinders in the car (cyl)
tapply(mtcars$mpg, mtcars$cyl, mean)
sapply(split(mtcars$mpg,mtcars$cyl),mean)

#quiz_4
dat <-lapply(split(mtcars$mpg,mtcars$cyl),mean)
abs(dat$`8`-dat$`4`)
#11.56364

#quiz_5
debug(ls)
#Execution of 'ls' will suspend at the beginning of the function and you will be in the browser.