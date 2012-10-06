        # Write a function that reads a directory full of files and reports the number of
        # completely observed cases in each data file.
        # The function should return a data frame
        # where the first column is the name of the file and the
        # second column is the number of complete cases.
        # A prototype of this function follows


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

        NL <- "\n"
        ID <- "id"
        NOBS <- "nobs"

        idVector <- vector(mode = "numeric", length = 0)
        nobVector <- vector(mode = "numeric", length = 0)

        for ( i in id ) {
        fullname <- create_fullname ( i, directory )

        ## extract from csv and create a frame

        air_data <- get_data( fullname )

        ## omit lines with NA
        ds_na_omit <- na.omit( air_data )

        # add elements to  vector
        idVector <- append(idVector, as.numeric(i) )
        nobVector<- append( nobVector, as.numeric(nrow(ds_na_omit)))
        }
         # create frame from the traversal of the id's and label columnes

        id <- as.numeric(idVector)
        nobs <- as.numeric(nobVector)
        nonNA_report <- data.frame( id, nobs ,stringsAsFactors = FALSE)



        return (nonNA_report)
      }
       ############### create_fullname ##############
        create_fullname <- function( i, directory ) {
        CSV <- ".csv"
        SLASH <- "/"
         ## Combine id and directory to create a full name
         idchar <- as.character(formatC(i, width=3, format='d', flag=0))
         filename <- paste0( idchar , CSV )
         # str(filename)
         returnValue<- paste0(directory, SLASH, filename)
        }

        ############### get_data ##############
       get_data <- function(fullname)  {
       # read the csv, frame it and drop the NAs
            na.omit( data.frame( ( (read.csv( fullname ))) ))
        }