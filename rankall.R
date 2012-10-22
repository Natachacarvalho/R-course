statelist <<-   
                c( "AK" ,"AL" ,"AR" ,"AZ" ,"CA" ,"CO" ,"CT" ,"DC" ,"DE" ,"FL" ,
                   "GA" ,"GU" ,"HI" ,"IA" ,"ID" ,"IL" ,"IN" ,"KS" ,"KY" ,"LA" ,
                   "MA" ,"MD" ,"ME" ,"MI" ,"MN" ,"MO" ,"MS" ,"MT" ,"NC" ,"ND" ,
                   "NE" ,"NH" ,"NJ" ,"NM" ,"NV" ,"NY" ,"OH" ,"OK" ,"OR" ,"PA" ,
                   "PR" ,"RI" ,"SC" ,"SD" ,"TN" ,"TX" ,"UT" ,"VT" ,"VI" ,"VA" ,
                   "WA" ,"WI" ,"WV" ,"WY")
NL <<- "NL"
outcomeSTRINGS <<- c( "heart attack","heart failure" ,"pneumonia")
outcomeCOLS <<- c("ha", "hf","pn")


rankall <- function( outcome_name, num = "best") {
  
  checkarguments( outcome_name, num)
  #readoutcome
  index <- match(outcome_name , outcomeSTRINGS)

  state.frame <- 
    data.frame( statelist, c(rep("NA",54)) ,stringsAsFactors = FALSE )
  colnames(state.frame ) <- c("state", "hospital")
 for ( state in statelist){
#for each state, let out be the subset on which you run order.data
   sub_outcome <- readcsvfile(state)
   # here is where I need to extract the name of the hotel
   order.data( sub_outcome, index, num)
   # match and get an index for the cell in the state.frame.
   # store the hospital name
 
  #  print the result and loop 
 } # end for each
}  # end of rankall

order.data<- function (out, index, num) {
    # order on the outcome and the hospital 
  out.sorted <- na.omit(out[order(out[index],out[5] ) ,])
  rtnvalue <- prepare.output( out.sorted, num )
}
########
prepare.output<-function ( sub.sorted, num){
 
  if (is.best(num) ) { rtnString <- sub.sorted[1, 5]; return(rtnString) } 
  else {
    if (is.worst(num) )    
                     { rtnString <- sub.sorted[nrow(sub.sorted), 5]; return(rtnString) } 
    else 
                     { rtnString <- sub.sorted[num,5] ; return(rtnString)   }
  } # end prepare.output
  
  #filename <- paste( outcome_name, ".csv")
  #write.csv(sub.sorted, filename)
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
