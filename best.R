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