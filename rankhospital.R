
NL <<- "\n"
outcomeSTRINGS <<- c( "heart attack","heart failure" ,"pneumonia")
outcomeINDEX <<-   c( 11, 17,23 )
rankhospital <- function( state, outcome, num="best") {
  
  # read outcome data 
  outcome <- readcsvfile(state)
  
  # check state/outcome are valid
  checkarguments(state, outcome_name )
  
  # return hospital name in that state with the given rank
  # for 30 day death rate
  
  
  
  
}

best5 <- function(state, outcome_name ){ 
  
  checkarguments(state, outcome_name )

  # need to looktable
  index <- match(outcome_name , outcomeSTRINGS)
  outcome_number <- outcomeINDEX[index]
  outcome <- readcsvfile(state)
  browse()
  
  #results <- parse.data(outcome,  outcome_number)
 
  #print(results[1:2])
  }

parse.data<- function (out, outcome_number) {
   
  out.sorted <- out[order(out[outcome_number] ,na.last=NA ),]
}
readcsvfile<-function(state){
  value<-read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  table <- data.frame( value, stringsAsFactors = FALSE)
  table[,11] <- as.numeric(gsub("Not Available", NA, table[,11]))
  table[,17] <- as.numeric(gsub("Not Available", NA, table[,17]))
  table[,23] <- as.numeric(gsub("Not Available", NA, table[,23]))
  table <- subset(table, State == state )
  #colnames(table[11] ) <- "mortHA"
  #colnames(table[17] ) <- "mortHF"
  #colnames(table[23] ) <- "mortPN"
  # how can I exclude some columns
  ha <- table[11]
  hf <- table[17]
  pn <- table[23]
  st <- table[7]
  ho <- table[2]
  browser()
  dfrm <- data.frame( c( ha,hf,pn,st, ho) )
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
