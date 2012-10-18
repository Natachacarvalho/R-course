
NL <<- "\n"
outcomeSTRINGS <<- c( "heart attack","heart failure" ,"pneumonia")
outcomeINDEX <<-   c( 11, 17,23 )
best <- function(state, outcome_name ){ 
  
  checkarguments(state, outcome_name )
  
  outcome <- readoutcome(state)
      names(outcome)[11] <- "mortHA"
  listname <- names(outcome)
  
  
  cat("name for ,11 ", listname, NL)
  
  #results <- parse.data(outcome,  outcome_name)
 
  }

parse.data<- function (out, outcome_name) {
  
  out.sorted <- out[order(,out$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack  )]

  #str(out.sorted)
}
readoutcome<-function(state){
  value<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  table <- data.frame( value, stringsAsFactors = FALSE)
  table[,11] <- as.numeric(table[,11]    )
  table[,17] <- as.numeric(table[,17]    )
  table[,23] <- as.numeric(table[,23]    )
  table <- subset(table, State == state)
  
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
