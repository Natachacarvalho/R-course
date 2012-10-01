getmonitor <- function(id, directory, summarize = FALSE) {

        ## author = pcsnow@gmail.com

        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'summarize' is a logical indicating whether a summary of
        ## the data should be printed to the console; the default is
        ## FALSE
        
        ## Your code here

        ## Constants

        NL <- "\n"
        CSV <- ".csv"
        SLASH <- "/"

        ## Combine id and directory to create a full name
        idchar <- as.character(formatC(id, width=3, format="d", flag=0))
        filename <- paste0( idchar , CSV )
        fullname <- paste0(directory, SLASH , filename)

        ## extract from csv and create a frame
        air_data <- data.frame(list(read.csv( fullname )))

        ## Create a summary if needed
        if (summarize ) {
            summary( air_data)
        }

        ## Print the frame
        print (air_data)


}