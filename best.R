best <- function(state, outcome_name ){ 
  
   if (!checkstate(state))  stop ("invalid state")
  
  print ( str( outcome_name ) )
  
  }

checkstate<- function(state) {
  if (!is.character(state) |  nchar(state) != 2 ){ return (
    return (
}