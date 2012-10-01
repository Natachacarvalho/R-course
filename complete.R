        # Write a function that reads a directory full of files and reports the number of
        # completely observed cases in each data file.
        # The function should return a data frame
        # where the first column is the name of the file and the
        # second column is the number of complete cases.
        # A prototype of this function follows
        NL <- "\n"
        CSV <- ".csv"
        SLASHSLASH <- "/"
        ID <- "id"
        NOBS <- "nobs"


        createfullname <- function (id, directory) { }


         checkNA <- function( air_data ) {

           countNA <- 0

           for ( i in air_data) {

           } #nd FOR

        }

complete <- function(directory, id = 1:332) {
        ## directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## id is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where id is the monitor ID number and 'nobs' is the
        ## number of complete cases

        #  set up the report heading
        nonNA_report <- c( ID, NOBS )

        for ( i in id ) {

        ## Combine id and directory to create a full name
            idchar <- as.character(formatC(i, width=3, format='d', flag=0))
            filename <- paste0( idchar , CSV )
            fullname <- paste0(directory, SLASH, filename)

        ## extract from csv and create a frame

        air_data <- data.frame(list(read.csv( fullname )))

        ## omit lines with NA
        ds_na_omit <- na.omit( air_data )

        # add report to  vector
        report <- c( as.character(i),as.character(nrow(ds_na_omit)) )
        nonNA_report <- append( nonNA_report, report  , after = length(report)   )

        }
        # traversed the input and accumulated the results
        return (nonNA_report)
      }