## Developed by Yelena Cherdak
## This function is:
## creates and adds list of functions to the "matrix" object supplied by 
## the parameter
## using R S3 scope mechanism it allowes to the user to access cached values 
## of x and matrixInv ( inverse of x) objects
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
## if the value is not set, function  creates it using 'solve()' function and 
## sets it cache
## return type: inverse matrix 

cacheSolve <- function(x, ...) {
  
  ## attempting to get inverse matrix from cache
  inverseM <- x$getinverse()
  
  if(is.null (inverseM) ) {
    message("Inverse matrix :: creating and setting to cache ")
    ## getting data from the input parameter
    matrixX <- x$get()
    ## attempting to solve the matrix
    inverseM <- solve(matrixX,...)
    ##setting the inverse to cache
    x$setinverse(inverseM)
  }
  ##returnng the inverse
  inverseM
}

