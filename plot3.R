   part3 <- function( ) {
    NL <- "\n"

    conditionTitle <-c("Heart_Attack","Heart_Failure","Pneumonia")
    ylabValue <- "Frequency" ; xlabValue <- "30-day Death Rate"

    outcome <- data.frame(read.csv("outcome-of-care-measures.csv", colClasses= "character" ))

    outcome[,11] <- as.numeric(outcome[,11]    )

       # Why doesn't this work ? presence/absence of final , doesn't matter
           outcome2 <-  outcome[outcome$State >= 20,  ] 
    
        cat ("nrow/ncol " , nrow(outcome2), ncol(outcome2), NL)
     
     table(outcome2$State)

            #Pitchfx <- subset(pujols, start_speed > 0)
            #pitchfx <- subset(pujols, pujols$start_speed > 0)
        }