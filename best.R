
NL <<- "\n"
best <- function(state, outcome_name , num = "best"){ 

  checkarguments(state, outcome_name , num)
  #cat("OK",NL)
  outcome <- readoutcome()
  #print(str(outcome))
  states <- buildlistStates (outcome $State)
  table(outcome$State)
  smalltable<-data.frame(table(outcome$State),stringsAsFactors = FALSE)
  # todo colname
  colnames(smalltable)<-c("state","count")
  resulttable <-subsetsoutcomeState(smalltable)
 print (str(smalltable))
 print (str(resulttable))
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
checknum<-function(num) {
    if (is.numeric(num)) return (TRUE)   ##TODO need to check for match state count
    if (!is.character( num )) return (FALSE)   
    if (num == "best" | num == "worst"  )  return (TRUE)
    return (FALSE)
  }
checkarguments<-function (state, outcome_name , num ){
 
  if (!checkstate(state))  stop ("invalid state")
  if (!checkoutcome(outcome_name))  stop ("invalid outcome")
  if (!checknum(num))  stop ("invalid num")
}
readoutcome<-function(){
  value<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
}
buildlistStates <-function(States){  
  uniq <- unique(toupper(States))
  sorted<-order(uniq)
  return (sorted)
}
subsetsoutcomeState<- function(smalltable) {
  
  smallframe = data.frame
  # using subset function
  newdata <- subset(smalltable, count >= 20 )

}