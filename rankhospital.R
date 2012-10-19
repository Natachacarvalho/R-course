
NL <<- "\n"
outcomeSTRINGS <<- c( "heart attack","heart failure" ,"pneumonia")


rankhospital <- function( state, outcome_name, num="best") {
  
  # read outcome data 
  outcome <- readcsvfile(state)
  
  # check state/outcome are valid
  checkarguments(state, outcome_name )
  index <- match(outcome_name , outcomeSTRINGS)
  # return hospital name in that state with the given rank
  # for 30 day death rate
  sub  <- parse.data(outcome, index)
  cat("TODO : Implement rank/worst best allready works")
  cat("TODO : order hospital name alpha ")
  print(sub[1,1:5])
  cat("TODO : writefile needs work")
  writefile( c( sub[,5],sub[,5], sub[,index]))
}
writefile <- function( mydata) {    
  
  write.table(mydata, "mydata.csv", sep=",")  
}

parse.data<- function (out, index) {
  cat( "index = ", index, NL )
  out.sorted <- out[order(out[index] ,na.last=NA ),]
}

readcsvfile<-function(state){
  value<-read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  table <- data.frame( value, stringsAsFactors = FALSE)
  table[,11] <- as.numeric(gsub("Not Available", NA, table[,11]))
  table[,17] <- as.numeric(gsub("Not Available", NA, table[,17]))
  table[,23] <- as.numeric(gsub("Not Available", NA, table[,23]))
  table <- subset(table, State == state )
  ha <- table[11]; hf <- table[17]; pn <- table[23]; st <- table[7] ;ho <- table[2]
  dfrm <- data.frame( c( ha,hf,pn,st, ho) ,stringsAsFactors = FALSE)
  return(dfrm)
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
