#######################################################################################################################################################################
# Author: Jutair Rios
#Assigment:02
#######################################################################################################################################################################

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # it will store the inverse matrix
  invm <- NULL
  
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  
  # getting the returns of the row matrix
  get <- function() {
    x
  }
  
  # setinv sets the inv variable
  # should be used only by cacheSolve
  setinvm <- function(i) {
    invm <<- i
  }
  
  # getinv gets the cached inverse
  getinvm <- function() {
    invm
  }
  
  # return the special matrix
  list(set = set,
       get = get,
       setinvm = setinvm,
       getinvm= getinvm)    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # get the cached inverse
  invm <- x$getinvm()
  
  if(!is.null(invm)) {
    # if the inverse if actually cached, just return it
    message("getting cached inverse")
    return(invm)
  }
  
  # otherwise, calculate the inverse and cache it
  matrinv <- x$get()
  invm <- solve(matrinv, ...)
  x$setinvm(invm)
  
  return(invm)
}


## An example for the function makeCacheMatrix():
# matrinv <- makeCacheMatrix(matrix(1:4, 2, 2))
#
## An example for the function cacheSolve():
# cacheSolve(matrinv):
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
# cacheSolve(matrinv):
#getting cached inverse
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5