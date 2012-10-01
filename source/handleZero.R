handleZero<- function( id ) {

    # pad a numeric and return a 3 digit string

    manychars <- as.character(formatC(id, width=3, format="d", flag=0))

    return (manychars)

}