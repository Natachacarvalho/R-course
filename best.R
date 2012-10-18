
NL <<- "\n"
outcomeSTRINGS <<- c( "heart attack","heart failure" ,"pneumonia")
outcomeINDEX <<-   c( 9,11, 17,23 )
best <- function(state, outcome_name ){ 
  
  checkarguments(state, outcome_name )
  
  outcome <- readoutcome(state)
      
  colnames(outcome)[11] <- "mortHA"
  
  
  results <- parse.data(outcome,  outcome_name)
  ###################### DEBUG ####################### 
  writefile(subset( results, select=c( Hospital.Name, mortHA))  )
  print(results[1,2])
  }

parse.data<- function (out, outcome_name) {
  # out$Hospital.Name,
  out.sorted <- out[order(out$mortHA  ),]
}
readoutcome<-function(state){
  value<-read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  table <- data.frame( value, stringsAsFactors = FALSE)
  table[,11] <- as.numeric(gsub("Not Available", NA, table[,11]))
  table[,17] <- as.numeric(gsub("Not Available", NA, table[,17]))
  table[,23] <- as.numeric(gsub("Not Available", NA, table[,23]))
  table <- subset(table, State == state)
  return(table)
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
  if (nchar(state) == 2 ) { return(TRUE) }
  
  
  else { return (FALSE) } #should not occur
}
writefile <- function( mydata) {    
  write.table(mydata, "mydata.txt", sep="\t")  
}
is.state<- function (state){
  statelist = c( "AK" ,"AL" ,"AR" ,"AZ" ,"CA" ,"CO" ,"CT" ,"DC" ,"DE" ,"FL" ,
                 "GA" ,"GU" ,"HI" ,"IA" ,"ID" ,"IL" ,"IN" ,"KS" ,"KY" ,"LA" ,
                 "MA" ,"MD" ,"ME" ,"MI" ,"MN" ,"MO" ,"MS" ,"MT" ,"NC" ,"ND" ,
                 "NE" ,"NH" ,"NJ" ,"NM" ,"NV" ,"NY" ,"OH" ,"OK" ,"OR" ,"PA" ,
                 "PR" ,"RI" ,"SC" ,"SD" ,"TN" ,"TX" ,"UT" ,"VT" ,"VI" ,"VA" ,
                 "WA" ,"WI" ,"WV" ,"WY")
  
}
