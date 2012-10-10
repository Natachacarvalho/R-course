               part2 <- function () {

    NL <- "\n"
    
    conditionTitle <-c("Heart_Attack","Heart_Failure","Pneumonia")    
    ylabValue <- "Frequency" ; xlabValue <- "30-day Death Rate"
    
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses= "character" )

    Heart_Attack <-  as.numeric (  outcome.outcome[,11] ) 
    Heart_Failure <- as.numeric (  outcome[,17] )
    Pneumonia <-     as.numeric (  outcome[,23] )
    
    condition <-data.frame( Heart_Attack,Heart_Failure,Pneumonia)
    str(condition)
    attach(condition)
    
    median <- c( mean(condition[,1] ,na.rm = TRUE ),
                 mean(condition[,2] ,na.rm = TRUE ),
                 mean(condition[,3] ,na.rm = TRUE ) )
     
       rangeHA <- range( condition[,1], na.rm = TRUE )  
       rangeHF <- range( condition[,2], na.rm = TRUE )
       rangePN <- range( condition[,3], na.rm = TRUE )
     

       hist(condition[,1] ,  main=conditionTitle[1] , xlab = xlabValue ,xlim=rangeHA )
       abline( h = median[1] , v = 0 , col = 2)
    
       hist(condition[,2] ,  main=conditionTitle[2] , xlab = xlabValue ,xlim=rangeHF )       
       abline( h = median[2] , v = 0 , col = 3)
    
       hist(condition[,3] ,  main=conditionTitle[3] , xlab = xlabValue ,xlim=rangePN ) 
       abline( h = median[3] , v = 0 , col = 4)
       par ( mfrow =c(3,1))
    
       detach(condition)
    }