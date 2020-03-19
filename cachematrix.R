## Put comments here that give an overall description of what your functions do

## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.


## Write a short comment describing this function

## The first function, makeCacheMatrix creates a matrix object
## that caches a square invertible matrix "x" 
## which is really a list containing a function to 
## set the value of the matrix
## get the value of the matrix
## set the inverse matrix
## and get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inverse <<- solveMatrix
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## However, it first checks to see 
## if the inverse has already been calculated
## (and the matrix has not changed). 
## If so,  the cacheSolve should retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)){
     message("getting cached data")
     return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse      
}

