#cat(NL, "TODO : Implement rank/worst best allready works", NL)
#cat(NL,"TODO : order hospital name alpha ",NL)
#cat(NL,"TODO : writefile needs work",NL)
NL <<- "\n"
WORST <<- FALSE
BEST  <<- FALSE
RANK <<- FALSE

RANKNUMBER <<- "NA"
outcomeSTRINGS <<- c( "heart attack","heart failure" ,"pneumonia")
outcomeCOLS <- c("ha", "hf","pn")

rankhospital <- function( state, outcome_name, num="best") {
  # read outcome data 
  sub_outcome <- readcsvfile(state)
  
  # check state/outcome are valid
  checkarguments(state, outcome_name, num) # states were subsetted
   
  index <- match(outcome_name , outcomeSTRINGS)
  column_name = outcomeCOLS[index]
  # return hospital name in that state with the given rank  # for 30 day death rate

  sub.sorted<- order.data(out, index)
 
  
   if (is.best(num) ) {
       rtnString <- sub.sorted[1, 5]
       print(rtnString)
       return("best")
         }
  if (is.worst(num) ) {
    cat("TODO FIX worst case",NL)
    rtnString <- sub.sorted[1, 5]
    print(rtnString)
    return("worst")
  }
  cat("num =", num , NL)
  cat("nrow =", nrow(sub.sorted) , NL)

     rtnString <- sub.sorted[num,5]
  
     print(rtnString) 
  
     all <- printall( sub_sorted, num)
     return("all")
}

printall<-function( dfrm, num, index) {
  while ( i <= nrow(dfrm)) {
    ## state, outcome, hos
    cat( dfrm[i,4], ",",dfrm[i,index], ",", dfrm$ho ,NL)
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

writefile <- function( mydata) {    
  write.table(mydata, "mydata.txt", sep="\t")  
}

checkarguments<-function (state, outcome_name,num ) { 
  if (!is.outcome(outcome_name))  stop ("invalid outcome")
  if (!is.state(state)) stop ("invalid state")
  if (!is.validrank(num)) stop( "is invalid num")
 
}
is.best <- function(num){ 
  if (num =="best") 
  { BEST<-TRUE; cat("best = true", NL); return(TRUE) }
                          else return(FALSE)
                        }
is.worst <- function(num){
  if (num =="worst") { WORST<-TRUE;  cat("worst = true", NL);return(TRUE) } 
                          else return(FALSE)
                          }

is.validrank<-function(num) {
  
  if( is.best (num) | is.worst(num)) {
    cat ( "is not rank", NL)
    return (TRUE)
  }
  
  
  # Check the numeric value since it is not best/worst
  rank.number <- as.numeric(num) 
  if(is.na(rank.number)) stop("invalid rank")
  RANKNUMBER <-rank.number
  RANK<-TRUE
  cat("(is.valid)rank.number = ", rank.number, NL)
  # 
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