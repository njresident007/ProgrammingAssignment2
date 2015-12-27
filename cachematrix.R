## Developed by Yelena Cherdak
## This function is:
## This function creates adds list of functions to the "matrix" object supplied by parameter
## using R S3 scope mechanism it allowes to the user to access cached values of x and matrixInv objects
##

makeCacheMatrix <- function(x = matrix()) {
  matrixInv <- NULL
  set <- function(y) {
    x <<- y
    matrixInv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) matrixInv <<- inv
  getinverse<- function() matrixInv
  list( set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function checks if object of type makeCacheMatrix has inverse value 
## set in cache
## if the value is not set, functions sets it
## return type: inverse matrix 

cacheSolve <- function(x, ...) {
  inverseM = NULL
  inverseM <- x$getinverse
  if(is.null (inverseM) & !is.null(x)) {
    inverseM <- solve(x)
    x.setinverse(inverseM)
  }
  return(inverseM)
}
