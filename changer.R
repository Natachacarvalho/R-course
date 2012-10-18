#create a table and then change the column name

# R CookBook 12.12 pg 323
cookbook12.12<- function(){


dfrm <- buildFrame() 
v<-order(dfrm[1])
print( v )

             
 dfrm <- dfrm[ order(dfrm[1]),]

  print(dfrm)
}
# R CookBook 12.13 pg 324
cookbook12.13<- function(){
  
  dfrm <- buildFrame()
  #order(dfrm$month,dfrm$day)
  dfrm <- dfrm[order(dfrm$month,dfrm$day),] #notice the final comma
  print(dfrm)
  }


buildFrame <- function(){
  month <- c( 7,8,8,6,7)
  day<-c(11,10,25,27,22)
  outcome<-c("Win", "Lose", "Tie", "Tie", "Win")
  dfrm <- data.frame( month,day,outcome)
}