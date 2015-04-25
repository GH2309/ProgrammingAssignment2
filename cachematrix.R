# First input (e.g. MYMATRIX is a matrix;  subsequently x<- make CacheMatrix(MYMATRIX) turns it into a list.  
# > x won't print a result, but x$get () will;  class(x$get()) gives a matrix again.  
#
#Similarly; cacheSolve(MYMATRIX) returns an error in command line,   "Error in x$getInverse : $ operator is invalid for atomic vectors"
  #> x<-cacheSolveMatrix(MYMATRIX) ==> class(x) is a list; whereas class(MYMATRIX) was matrix
  #This was a sticking point for me !!  
  #
  # Final result is returned as a matrix;  
  # if ANTI is the inverse, then ANTI %*% MYMATRIX  will yield [1 0]
  #                                                            [0 1]
  #
  #

makeCacheMatrix <- function(x = matrix()) {
  
  
  M <- NULL
  set <- function(y) {
    
    x <<- y
    M <<- NULL
  }
  
  get <- function() x
  # renamed mean to inverse from example script
  set_inv <- function(inverse) M <<- inverse 
  get_inv <- function() M
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}  


cacheSolve <- function(x, ...) {
  #say if GEOFF was the original matrix; cacheSolve(GEOFF) returns an error in command line, 
  #> x<-cacheSolveMatrix(GEOFF) ==> class(x) is a list; whereas class(GEOFF) was matrix
  #This was a sticking point for me !!  
  #> dataM<- x$get() ==> then class(dataM) is a matrix again... 
  #
  # modified from example...  
  
  M <- x$get_inv()
  
  if (!is.null(M)){
    message("getting cached data")
    return(M)
  } 
    #else ...
  
  dataM <- x$get()  ## where X was a list; dataM is now a matrix; then can use solve() function.
    # message("dataM:- ", dataM)  ## this line for logging
  M <- solve(dataM, ...) 
  
  x$set_inv(M)
  
  return(M)
}
## MY CHEAT SHEET:
# ref 1 : http://www.r-tutor.com/r-introduction/matrix/matrix-construction
# -- make matrix ie.  GEOFF = matrix(c(2, 4, 3, 1),nrow=2)
# GEOFF then shows [2 3]
#                  [4 1]
# anti<- solve(GEOFF)
# anti %*% GEOFF # gives:
#  [,1]          [,2]
#[1,]    1 -5.551115e-17
#[2,]    0  1.000000e+00... should return [1 0 / 0 1]... certain matrices can't be inverted (?!)

# example usage; 
# x<- makeCacheMatrix(GEOFF)
# x != GEOFF 
#> x # returns error/ list of envi
#> x$get()  # shows same matrix as GEOFF
#> cacheSolve(GEOFF) # gives error;   "Error in x$getInverse : $ operator is invalid for atomic vectors"


## [Put comments here that describe what your functions do]
##so, matrix is called "M" (use capitals)
## inverse is called cacheSolve

