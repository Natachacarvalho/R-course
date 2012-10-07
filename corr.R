

corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        ############### Constants ##############
        NL <- "\n"
        ID <- "id"
        NOBS <- "nobs"
        # create some static vectors, although I may not need them
        idVector <- vector(mode = "character", length = 0)
        nobVector <- vector(mode = "character", length = 0)
        sulphurVector<- vector( mode="numeric", length=0)
        nitrateVector<- vector( mode="numeric", length=0)
        corrVector<- vector( mode="numeric", length=0)
        ErrorVector<- vector( mode="numeric", length=0)   # if no correlations exceeding threshold
        id<- seq(1:332)
        for ( i in id ) {

        ## parse the file names to be processed
         fullname <- create_fullname ( i , directory )

        ## extract from csv and create a frame
        ds_na_omit <- get_data( fullname)

        ## run a cor (relation) Maybe this should be a function too
        str(threshold)
        if (threshold  >=  length ( ds_na_omit ) )   {
            compute_cor( ds_na_omit )
        }

        }
        ## Done return the vector of correlation or ErrorVector
        return (ds_na_omit)
        }
        ## end of corr

############### Functions ##############
############### get_data ##############
       get_data <- function(fullname)  {
       # read the csv, frame it and drop the NAs
            na.omit( data.frame( ( list(read.csv( fullname ))) ))
        }

############### create_fullname ##############
        create_fullname <- function( i, directory ) {
        CSV <- ".csv"
        SLASH <- "/"
         ## Combine id and directory to create a full name
         idchar <- as.character(formatC(i, width=3, format='d', flag=0))
         filename <- paste0( idchar , CSV )
         returnValue<- paste0(directory, SLASH, filename)
        }
############### create_fullname ##############
        compute_cor <- function( ds_na_omit )
        ds_na_omit$corr <-  cor( as.numeric(ds_na_omit$sulfate), y=ds_na_omit$nitrate )

