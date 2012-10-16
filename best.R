
NL <<- "\n"
best <- function(state, outcome_name ){ 
  
  checkarguments(state, outcome_name )
 
  outcome <- readoutcome()

  }


checkstate<- function(state) {
  if (!is.character(state) ){ return (FALSE) }  
  if (nchar(state) == 2 ) { 
       return(TRUE)}
    else { return (FALSE) } #should not occur
}
checkoutcome<-function(outcome_name) {
  if (outcome_name == "heart attack" 
      | outcome_name == "heart failure" 
      | outcome_name == "pneumonia" )
    return (TRUE)   
  else  return (FALSE)
} 

checkarguments<-function (state, outcome_name ) { 
  if (!checkoutcome(outcome_name))  stop ("invalid outcome")
  if (!checkstate(state)) top ("invalid state")
}
readoutcome<-function(){
  value<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
}
checkstate<- function(state) {
  if (!is.character(state) ){ return (FALSE) }  
  if (nchar(state) == 2 ) { 
    return(TRUE)}
  else { return (FALSE) } #should not occur
}

