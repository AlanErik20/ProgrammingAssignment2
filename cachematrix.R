## Put comments here that give an overall description of what your
## functions do

##We focus on that task, writing two functions.
##"makeCacheMatrix" and "cacheSolve" that can cache the reverse of a matrix.

## Write a short comment describing this function

##makeCacheMatrix is a function that can create a matrix that can store the input of a square matrix that can be invertible

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

##cacheSolve calculates the inverse of the array returned by the makeCacheMatrix function. With the inverse calculated, the cacheSolve function should return the inverse of the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
