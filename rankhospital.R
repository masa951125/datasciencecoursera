
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