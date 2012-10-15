best <- function(state, outcome_name , num = "best"){ 
  
  checkarguments(state, outcome_name , num)

  
  }

checkstate<- function(state) {
  NL <- "\n"
  if (!is.character(state) ){ return (FALSE) }
  
  if (nchar(state) == 2 ) { 
       return(TRUE)
    }
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