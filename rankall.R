
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