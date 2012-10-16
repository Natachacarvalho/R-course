
NL <<- "\n"
best <- function(state, outcome_name ){ 
  
  checkarguments(state, outcome_name )
  
  debug(readoutcome)
  outcome <- readoutcome()
  
  print("before str")
  str(outcome)
  #diminished.outcome <- parse.data(outcome, state,outcome_name)
  print ("after parse")
  }
readoutcome<-function(){
  value<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  table <- data.frame( value, stringsAsFactors = FALSE)
  table[,11] <- as.numeric(table[,11]    )
  table[,17] <- as.numeric(table[,17]    )
  table[,23] <- as.numeric(table[,23]    )
  str(table)
  return(table)
  
  # add selection / subset
}

checkstate<- function(state) {
  if (!is.character(state) ){ return (FALSE) }  
  if (nchar(state) == 2 ) { 
       return(TRUE)}
    else { return (FALSE) } #should not occur
}
checkoutcome<-function(outcome_name) {
  if  ( outcome_name == "heart attack" 
      | outcome_name == "heart failure" 
      | outcome_name == "pneumonia" )
    return (TRUE)   
  else  return (FALSE)
} 

checkarguments<-function (state, outcome_name ) { 
  if (!checkoutcome(outcome_name))  stop ("invalid outcome")
  if (!checkstate(state)) top ("invalid state")
}

checkstate<- function(state) {
  if (!is.character(state) ){ return (FALSE) }  
  if (nchar(state) == 2 ) { 
    return(TRUE)}
  else { return (FALSE) } #should not occur
}
