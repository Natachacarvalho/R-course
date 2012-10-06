testframe <- function() {
 X <- c( 10,20,25,20 )
 Y <- c( 152,458,698,587 )

 test <- data.frame ( X, Y )
 


test$Z <- test$X + 5 * test$Y

str ( test)
}
#'data.frame':	4 obs. of  3 variables:
$ $ X: num  10 20 25 20
# $ Y: num  152 458 698 587
# $ Z: num  770 2310 3515 2955

testmatrix <- function() {
 X <- c( 10,20,25,20 )
 Y <- c( 152,458,698,587 )

 test <- matrix( X,Y)
 str(test)

  test$Z <- test$X + 5 * test$Y
  str(test)

}