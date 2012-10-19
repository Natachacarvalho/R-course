
NL <<- "\n"
outcomeSTRINGS <<- c( "heart attack","heart failure" ,"pneumonia")
outcomeINDEX <<-   c( 11, 17,23 )

best <- function(state, outcome_name ){ 
  
  checkarguments(state, outcome_name )

  # need to looktable
  index <- match(outcome_name , outcomeSTRINGS)
  outcome_number <- outcomeINDEX[index]
  outcome <- readoutcome(state)
  outcomeName <- outcome[]
  results <- parse.data(outcome,  outcome_number)
 
  print(results[1,2])
  }

parse.data<- function (out, outcome_number) {
   
  out.sorted <- out[order(out[outcome_number] ,na.last=NA ),]
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

checkarguments<-function (state, outcome_name ) { 
  if (!is.outcome(outcome_name))  stop ("invalid outcome")
  if (!is.state(state)) stop ("invalid state")
}
writefile <- function( mydata) {    
  write.table(mydata, "mydata.txt", sep="\t")  
}

is.state<- function (state){
  statelist = 
    c( "AK" ,"AL" ,"AR" ,"AZ" ,"CA" ,"CO" ,"CT" ,"DC" ,"DE" ,"FL" ,
       "GA" ,"GU" ,"HI" ,"IA" ,"ID" ,"IL" ,"IN" ,"KS" ,"KY" ,"LA" ,
       "MA" ,"MD" ,"ME" ,"MI" ,"MN" ,"MO" ,"MS" ,"MT" ,"NC" ,"ND" ,
       "NE" ,"NH" ,"NJ" ,"NM" ,"NV" ,"NY" ,"OH" ,"OK" ,"OR" ,"PA" ,
       "PR" ,"RI" ,"SC" ,"SD" ,"TN" ,"TX" ,"UT" ,"VT" ,"VI" ,"VA" ,
       "WA" ,"WI" ,"WV" ,"WY")
  returnValue <-(! is.na(match( state, statelist)))
  }
is.outcome<-function(outcome_name) {
  returnValue <-(! is.na(match( outcome_name, outcomeSTRINGS)))
}
