part2 <- function() {

    NL <- "\n"
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses= "character" )




    Heart_Attack <- as.numeric ( c( outcome[,11 ] ) )
    rangeHA <- range( Heart_Attack , na.rm = TRUE )
            cat("HA range : ", rangeHA , NL )
    Heart_Failure <- as.numeric ( c(outcome[,17] )  )

    rangeHF <- range( Heart_Failure ,na.rm = TRUE )
            cat("HF range : ", rangeHF, NL)
    Pneumonia <- as.numeric ( c( outcome[,23] ) )
    rangePN <- range( Pneumonia ,    na.rm = TRUE )
            cat("PN range : ", rangePN , NL)

    condition <-c(Heart_Attack,Heart_Failure,Pneumonia)

    conditionTitle <-c("Heart Attack","Heart Failure","Pneumonia")

    #
    ylabValue <- "Frequency" ; xlabValue <- "30-day Death Rate"

    cat("ncol(condition : ",  ncol(condition), NL  )
    #for ( i in ncol(condition)  )      {

        par ( mfrow =c(3, 1), xlab = xlabValue )
        hist(condition[1] ,  main=conditionTitle[1] )
        hist(condition[2] ,  main=conditionTitle[2] )
        hist(condition[3] ,  main=conditionTitle[3] )
    #}
}



