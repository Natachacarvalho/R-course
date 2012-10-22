
NL <<- "\n"

outcomeSTRINGS <<- c( "heart attack","heart failure" ,"pneumonia")
outcomeCOLS <- c("ha", "hf","pn")
statelist = 
  c( "AK" ,"AL" ,"AR" ,"AZ" ,"CA" ,"CO" ,"CT" ,"DC" ,"DE" ,"FL" ,
     "GA" ,"GU" ,"HI" ,"IA" ,"ID" ,"IL" ,"IN" ,"KS" ,"KY" ,"LA" ,
     "MA" ,"MD" ,"ME" ,"MI" ,"MN" ,"MO" ,"MS" ,"MT" ,"NC" ,"ND" ,
     "NE" ,"NH" ,"NJ" ,"NM" ,"NV" ,"NY" ,"OH" ,"OK" ,"OR" ,"PA" ,
     "PR" ,"RI" ,"SC" ,"SD" ,"TN" ,"TX" ,"UT" ,"VT" ,"VI" ,"VA" ,
     "WA" ,"WI" ,"WV" ,"WY")

rankhospital <- function( state, outcome_name, num="best") {
  # read outcome data 
  sub_outcome <- readcsvfile(state)
  
  # check state/outcome are valid
  checkarguments(state, outcome_name, num) # states were subsetted
   
  index <- match(outcome_name , outcomeSTRINGS)
  column_name = outcomeCOLS[index]
  # return hospital name in that state with the given rank  # for 30 day death rate
return("before order.data")
  sub.sorted<- order.data(sub_outcome, index)
  
   if (is.best(num) ) {
       rtnString <- sub.sorted[1, 5]
       print(rtnString)
       
         } else {
  if (is.worst(num) ) {    
    rtnString <- sub.sorted[nrow(sub.sorted), 5]
    print(rtnString)
    }else {

     
     rtnString <- sub.sorted[num,5] 
     print(rtnString) 
     #filename <- paste( outcome_name, ".csv")
     #write.csv(sub.sorted, filename)
}
         }
}

printall<-function( dfrm, num, index) {
  i <- 1
  while ( i <= nrow(dfrm)) {
    ## state, outcome, hos
    print( dfrm[i,4], ",",dfrm[i,index], ",", dfrm$ho ,NL)
  }
}

order.data<- function (out, index) {

  # order on the outcome and the hospital 
  out.sorted <- na.omit(out[order(out[index],out[5] ) ,])
 
 }

readcsvfile<-function(state){
  value<-read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  table <- data.frame( value, stringsAsFactors = FALSE)
  table[,11] <- as.numeric(gsub("Not Available", NA, table[,11]))
  table[,17] <- as.numeric(gsub("Not Available", NA, table[,17]))
  table[,23] <- as.numeric(gsub("Not Available", NA, table[,23]))
  table <- subset(table, State == state )
  ha <- table[11]; hf <- table[17]; pn <- table[23]; st <- table[7] ;ho <- table[2]
  dfrm <- data.frame( c( ha,hf,pn,st,ho) ,stringsAsFactors = FALSE)
 # dfrm <- data.frame( c( ho,st,pn,hf,ha) ,stringsAsFactors = FALSE)
  colnames(dfrm) <- c("ha", "hf", "pn", "st","ho")  
  return(dfrm)
}


checkarguments<-function (state, outcome_name,num ) { 
  if (!is.outcome(outcome_name))  stop ("invalid outcome")
  if (!is.state(state)) stop ("invalid state")
  if (!is.validrank(num)) stop( "is invalid num")
 
}
is.best <- function(num){ 
  if (num =="best") { BEST<-TRUE;  return(TRUE) }
                          else return(FALSE)
                        }
is.worst <- function(num){
  if (num =="worst") { WORST<-TRUE; return(TRUE) } 
                          else return(FALSE)
                          }

is.validrank<-function(num) { 
  if( is.best (num) | is.worst(num)) {
    return (TRUE)
  }
   
  # Check the numeric value since it is not best/worst
  rank.number <- as.numeric(num) 
  if(is.na(rank.number)) stop("invalid rank")
  RANKNUMBER <-rank.number
  RANK<-TRUE
  return(TRUE)
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
rev_order <- function (outcome) {
  nr <- nrow(outcome)
  ordered =  (outcome[order(outcome[5],na.last=NA),] )
  reversed<-(rev( ordered ))
  # how many states
  #out[order(out[index] ,na.last=NA ),]
}