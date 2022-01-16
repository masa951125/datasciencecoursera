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

############################################################################
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

#############################################################################
#program assignment
#1
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

head(outcome)
ncol(outcome)
names(outcome)
class(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)

outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])

#2
outcome[,7] #state
outcome[,11] #heart attack
outcome[,17] #heart failure
outcome[,23] #pneumonia
outcome[,2] #hospital name
outcome$Hospital.Name

f_outcome <- outcome %>% filter(outcome[,7]=="TX")
f_outcome[,17] <- as.numeric(f_outcome[,17])
min(f_outcome[,17],na.rm=T)
as.numeric(f_outcome[,17])

d<-  f_outcome %>% 
  filter(f_outcome[,17]==min(f_outcome[,17])) %>% 
  select("Hospital.Name")
d
d[order(d),][1]

#####
best <- function(state, outcome){
  df <- read.csv("outcome-of-care-measures.csv",
                 na.strings= "Not Available",stringsAsFactors=FALSE)
  `%ni%` <- Negate(`%in%`)

  if (state %ni% df[,7]){
    stop("invalid state")}
  else if(outcome %ni% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")}
    
  else{
    df <- df %>% filter(df[,7]==state)

    if(outcome =="heart attack"){
      h_name <- df %>% 
        filter(df[,11]==min(df[,11],na.rm = T)) %>% 
        select("Hospital.Name")
      name <- h_name[order(h_name),][1]}
    
    else if(outcome =="heart failure"){
      h_name <- df %>% 
        filter(df[,17]==min(df[,17],na.rm = T)) %>% 
        select("Hospital.Name")
      name <- h_name[order(h_name),][1]}
    
    else if(outcome =="pneumonia"){
      h_name <- df %>% 
        filter(df[,23]==min(df[,23],na.rm = T)) %>% 
        select("Hospital.Name")
      name <- h_name[order(h_name),][1]}}
    
  print(name)  
}

#test
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
######

#3
outcome <- read.csv("outcome-of-care-measures.csv",
                    na.strings= "Not Available",stringsAsFactors=FALSE)

rank <- outcome %>% filter(State =="TX")%>%
  arrange(Hospital.Name) %>%
  mutate(rate = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Rank = row_number(rate))

sum(!is.na(rank$Rank))
n <- rank %>% filter(rank$Rank==4)
n$Hospital.Name

rankhospital("TX", "heart failure", 4)

#####
rankhospital <- function(state, outcome, num ="best"){
  df <- read.csv("outcome-of-care-measures.csv",
                 na.strings= "Not Available",stringsAsFactors=FALSE)
  `%ni%` <- Negate(`%in%`)
  
  if (state %ni% df[,7]){
    stop("invalid state")}
  else if(outcome %ni% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")}
  
  else{
    df <- df %>% filter(df[,7]==state)
    
    if(outcome =="heart attack"){
      rank <- df %>% filter(State ==state)%>%
        arrange(Hospital.Name) %>%
        mutate(Rate =Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
               Rank = row_number(Rate)) 
        
        if(num=="best"){num <-1}
        else if(num=="worst"){num = sum(!is.na(rank$Rank))}
        
        n <- rank %>% filter(rank$Rank==num)
        name <- n$Hospital.Name}
    
    if(outcome =="heart failure"){
      rank <- df %>% filter(State ==state)%>%
        arrange(Hospital.Name) %>%
        mutate(Rate =Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, 
               Rank = row_number(Rate))
      
      if(num=="best"){num <-1}
      else if(num=="worst"){num = sum(!is.na(rank$Rank))}

      n <- rank %>% filter(rank$Rank==num)
      name <- n$Hospital.Name}
      
    if(outcome =="pneumonia"){
      rank <- df %>% filter(State ==state)%>%
        arrange(Hospital.Name) %>%
        mutate(Rate =Hospital.30.Day.Readmission.Rates.from.Pneumonia, 
               Rank = row_number(Rate))
      
      if(num=="best"){num <-1}
      else if(num=="worst"){num = sum(!is.na(rank$Rank))}

      n <- rank %>% filter(rank$Rank==num)
      name <- n$Hospital.Name}}

    if (length(name)==0){print(NA)}
    else{print(name)}  
}
#test
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

#4

rankall <- function(outcome, num ="best"){

  #premises(%ni%, invalid outcome)
  df <- read.csv("outcome-of-care-measures.csv",
                      na.strings= "Not Available",stringsAsFactors=FALSE)
  `%ni%` <- Negate(`%in%`)
  if(outcome %ni% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")}

  #outcome
  if (outcome =="heart attack"){
    rank <- df %>% 
      group_by(State)%>%
      arrange(Hospital.Name)%>%
      mutate(Rank = row_number(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))%>%
      ungroup() %>%
      select(Hospital.Name,State, Rank)

  }else if(outcome =="heart failure"){
    rank <- df %>% 
      group_by(State)%>%
      arrange(Hospital.Name)%>%
      mutate(Rank = row_number(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))%>%
      ungroup() %>%
      select(Hospital.Name,State, Rank)
    
  }else if(outcome =="pneumonia"){
    rank <- df %>% 
      group_by(State)%>%
      arrange(Hospital.Name)%>%
      mutate(Rank = row_number(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))%>%
      ungroup() %>%
      select(Hospital.Name,State, Rank)
  }
  #num
  hos_rank <- data.frame(matrix(rep(NA, 3), nrow=1))[numeric(0), ]
  colnames(hos_rank) <- c("Hospital.Name", "State", "Rank")
  states <- sort(c(unique(df$State)))
  
  if(num=="best"){
    num =1
    for(state in states){
      hos_rank <- rbind(hos_rank, filter(rank,Rank==num,State== state))
      }
  }else if (num=="worst"){
    for(state in states){
      hos_rank <- rbind(hos_rank,filter(rank,Rank==max(rank[rank$State== state,]$Rank,na.rm = T),State== state))
      }
  }else{
    for(state in states){
      if(nrow(filter(rank, Rank==num,State==state))==0){
        hos_rank <-rbind(hos_rank, data.frame(Hospital.Name=NA, State=state, Rank=num))
      }else{
        hos_rank <- rbind(hos_rank, filter(rank,Rank==num,State==state)) 
      }
      }
  }
  hos_rank <- hos_rank %>% 
    mutate(hospital= Hospital.Name, state= State) %>%
    select(hospital, state)
  hos_rank
}

#test
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

############################################################################
#Programming assessment quiz
#1
best("SC", "heart attack")
#"MUSC MEDICAL CENTER"

#2
best("NY", "pneumonia")
# "MAIMONIDES MEDICAL CENTER"

#3
best("AK", "pneumonia")
#"YUKON KUSKOKWIM DELTA REG HOSPITAL"

#4
rankhospital("NC", "heart attack", "worst")
#"WAYNE MEMORIAL HOSPITAL"

#5
rankhospital("WA", "heart attack", 7)
#"YAKIMA VALLEY MEMORIAL HOSPITAL"

#6
rankhospital("TX", "pneumonia", 10)
#"FORT DUNCAN MEDICAL CENTER"

#7
rankhospital("NY", "heart attack", 7)
#"BELLEVUE HOSPITAL CENTER"

#8
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
#"CASTLE MEDICAL CENTER"

#9
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
# "BERGEN REGIONAL MEDICAL CENTER"

#10
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
#"RENOWN SOUTH MEADOWS MEDICAL CENTER"
###


