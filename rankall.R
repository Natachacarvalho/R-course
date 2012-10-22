statelist <<-   
                c( "AK" ,"AL" ,"AR" ,"AZ" ,"CA" ,"CO" ,"CT" ,"DC" ,"DE" ,"FL" ,
                   "GA" ,"GU" ,"HI" ,"IA" ,"ID" ,"IL" ,"IN" ,"KS" ,"KY" ,"LA" ,
                   "MA" ,"MD" ,"ME" ,"MI" ,"MN" ,"MO" ,"MS" ,"MT" ,"NC" ,"ND" ,
                   "NE" ,"NH" ,"NJ" ,"NM" ,"NV" ,"NY" ,"OH" ,"OK" ,"OR" ,"PA" ,
                   "PR" ,"RI" ,"SC" ,"SD" ,"TN" ,"TX" ,"UT" ,"VT" ,"VI" ,"VA" ,
                   "WA" ,"WI" ,"WV" ,"WY")
NL <<- "\n"
outcomeSTRINGS <<- c( "heart attack","heart failure" ,"pneumonia")
outcomeCOLS <<- c("ha", "hf","pn")


rankall <- function( outcome_name, num = "best") {
  
  checkarguments( outcome_name, num)
  #readoutcome
  index <- match(outcome_name , outcomeSTRINGS)
  hospital <- c( rep("NA",54))
  state.frame <- 
    data.frame( hospital, statelist ,stringsAsFactors = FALSE )
    
 for ( state in statelist){
#for each state, let out be the subset on which you run order.data
   sub_outcome <- readcsvfile(state)
   # here is where I need to extract the name of the hotel
   sorted_outcome <- order.data( sub_outcome, index )
   # match and get an index for the cell in the state.frame.
   # store the hospital name
 
  #  print the result and loop 
   printed.output <- prepare.output( sorted_outcome, num )
   state.index <- match(state, state.frame[,2] )
   if (state.index == 0) stop ("invalid state index")
   state.frame[state.index, 1] <- printed.output
   
   #cat (  state.index, " ")
 } # end for each
  colnames(state.frame ) <- c( "hospital", "state")
  filename <- paste( outcome_name, ".csv")
  write.csv(state.frame, filename)
  return(state.frame)
}  # end of rankall

order.data<- function (out, index) {
    # order on the outcome and the hospital 
  out.sorted <- na.omit(out[order(out[index],out[5] ) ,])
  return(out.sorted)  
}
########
prepare.output<-function ( sub.sorted, num ){
 
  if (is.best(num) ) { rtnString <- sub.sorted[1, 5] ; return(rtnString) } 
  else {
    
    if (is.worst(num) )    
                     { rtnString <- sub.sorted[nrow(sub.sorted), 5]; return(rtnString) } 
    else 
                     { rtnString <-  sub.sorted[num,5];  return(rtnString)   }
  } # end prepare.output
  

}
#########################################################
checkarguments<-function ( outcome_name, num ) { 
  if (!is.outcome(outcome_name))  stop ("invalid outcome")
  if (!is.validrank(num)) stop( "is invalid rank" ) 
}

is.best <- function(num){ 
  if (num == "best") { BEST<-TRUE;  return(TRUE) }
  else return(FALSE)
} # end is best
is.worst <- function(num){
  if (num == "worst") { WORST<-TRUE; return(TRUE) } 
  else return(FALSE)
}

is.validrank<-function(num) { 
  if( is.best (num) | is.worst(num)) { return (TRUE)}
  
  # Check the numeric value since it is not best/worst
  rank.number <- as.numeric(num) 
  if(is.na(rank.number)) stop("invalid rank")
  return(TRUE)
}
  
is.outcome<-function(outcome_name) {
  returnValue <-(! is.na(match( outcome_name, outcomeSTRINGS)))
}
  
readcsvfile<-function( state){
  value<-read.csv("outcome-of-care-measures.csv", colClasses = "character")  
table <- data.frame( value, stringsAsFactors = FALSE)
table[,11] <- as.numeric(gsub("Not Available", NA, table[,11]))
table[,17] <- as.numeric(gsub("Not Available", NA, table[,17]))
table[,23] <- as.numeric(gsub("Not Available", NA, table[,23]))
table <- subset(table, State == state )
ha <- table[11]; hf <- table[17]; pn <- table[23]; st <- table[7] ;ho <- table[2]
dfrm <- data.frame( c( ha,hf,pn,st,ho) ,stringsAsFactors = FALSE)
colnames(dfrm) <- c("ha", "hf", "pn", "st","ho")  
return(dfrm)
}
