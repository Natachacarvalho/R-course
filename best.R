
NL <<- "\n"
best <- function(state, outcome_name , num = "best"){ 
  
  checkarguments(state, outcome_name , num)
  #cat("OK",NL)
  outcome <- readoutcome()
  #print(str(outcome))
  states <- buildlistStates (outcome$State)
  table(outcome$State)
  smalltable<-data.frame(table(outcome$State),stringsAsFactors = FALSE)
  colnames(smalltable)<-c("state","count")
  outcome2 <-subsetsoutcomeState(smalltable, num)
  print (str(smalltable))
  print (str(outcome2)) 
  makeplot( outcome2 )
  }

makeplot<-function(outcome2 ){
    fatal <- outcome2$count
    state <- outcome2$state
    boxplot( fatal ~ state)
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
checkarguments<-function (state, outcome_name ,num) { 
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
subsetsoutcomeState<- function(smalltable, num ) {
  criteria <- num
  cat("num="      , str(num), NL)
  cat("criteria" , str(criteria), NL)
  if ( num == "best")  criteria<- 1 
  if ( num == "worst")  criteria<- nrow(smalltable)
  
  cat("num=" , str(num), NL)
  cat("criteria" , str(criteria), NL)
  print(str(criteria))
  # using subset function
  outcome2 <- subset(smalltable, smalltable$count >= criteria )
  cat("outcome2.nrows = ", nrow(outcome2 ), NL)
  return (outcome2)
}