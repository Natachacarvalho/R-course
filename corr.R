

corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

        NL <- "\n"

        ID <- "id"
        NOBS <- "nobs"





        idVector <- vector(mode = "character", length = 0)
        nobVector <- vector(mode = "character", length = 0)
        sulphurVector<- vector( mode="numeric", length=0)
        nitrateVector<- vector( mode="numeric", length=0)
        corrVector<- vector( mode="numeric", length=0)
        id <- c(1:2 )                                        #Testing
        traceback()
        #debug(get_data)
        #debug(create_fullname)

        for ( i in id ) {

         fullname <- create_fullname ( i , directory )

        ## extract from csv and create a frame

        cat("fullname: ", fullname, NL)                ##DEBUG

        air_data <- get_data( fullname)
        ds_na_omit <- na.omit( air_data )
        cat("ds_na_omit:", NL)
        str(ds_na_omit)

        cat("howmany : ", length(ds_na_omit$sulfate), " , ", length(ds_na_omit$nitrate), NL)
        ds_na_omit$corr <-  cor( as.numeric(ds_na_omit$sulfate), y=ds_na_omit$nitrate )
        # did I get good data ?

        cat( "frame after cor : ", NL)
        str(ds_na_omit)

         }

         # create frame from the traversal of the id's and label columnes
           ds_na_omit <- data.frame( idVector,nobVector  )
           colnames( ds_na_omit ) <- c( ID, NOBS )

          # complete
        return (ds_na_omit)
      }        # end of corr

       get_data <- function(fullname)  {
            data.frame( ( list(read.csv( fullname ))) )
        }

        create_fullname <- function( i, directory ) {
        CSV <- ".csv"
        SLASH <- "/"
         ## Combine id and directory to create a full name
         idchar <- as.character(formatC(i, width=3, format='d', flag=0))
         filename <- paste0( idchar , CSV )
         returnValue<- paste0(directory, SLASH, filename)
        }

