#system.time
system.time(readLines("http://www.jhsph.edu"))

hilbert <- function(n) {
   i <- 1:n
   1 / outer(i - 1, i, "+")
}
x <- hilbert(1000)
system.time(svd(x))

#Rprof
Rprof()

Rprof(NULL)

summaryRprof()

#quiz1
set.seed(1)
rpois(5, 2)
# 1 1 2 4 1

#quiz2
rnorm()

#quiz3
#It ensures that the sequence of random numbers starts in a specific place and is therefore reproducible.

#quiz4
qpois()

#quiz5
set.seed(10)
x <- rep(0:1, each = 5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e
x
y

plot(x,y)
#Generate uniformly distributed random data

#quiz6
rbinom()

#quiz7
#the function call stack

#quiz8
library(datasets)
Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)

#100%

#quiz9
#It is the time spent by the CPU evaluating an expression

#quiz10
#user time is always smaller than elapsed time