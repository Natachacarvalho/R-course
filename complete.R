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


        heading <- c( ID, NOBS )

        nonNA <- data.frame( heading )


        for ( i in id ) {

        ## Combine id and directory to create a full name
        print(id)
            idchar <- as.character(formatC(i, width=3, format='d', flag=0))

            filename <- paste0( idchar , CSV )

            fullname <- paste0(directory, SLASH, filename)



        ## extract from csv and create a frame
        cat("fullname ", fullname, NL)
        air_list<-list(read.csv( fullname ))

        air_data <- data.frame( air_list)


        ds_na_omit <- na.omit( air_data )
        cat("index: ", i, NL)
        cat("ds_na_omit ",str(ds_na_omit) )
        cat("nrows ", length( ds_na_omit ) )


        # so I need to see how many of the obs have no na's
        # how many nobs   ro
        # add to id,nobs to vector nonNA
        #now need to print out nonNA that I collected
        }
     }