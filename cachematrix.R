## Programming Assignment 2: Lexical Scoping
## This pair of functions cache the inverse of a matrix to avoid 
## repeated expensive computations. Matrix inversion is 
## a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly.

## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse
## This function creates a special "matrix" object, which is really a list 
## containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
##
## The function uses lexical scoping to maintain state across function calls.
## The <<- operator is used to assign values to objects in an environment 
## different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # Initialize the inverse as NULL
  
  # Function to set a new matrix and reset the cached inverse
  set <- function(y) {
    x <<- y # Assign new matrix to parent environmen
    inv <<- NULL # Reset the cached inverse since matrix changed
  }
  
  
  # Function to get the current matrix
  get <- function() {
    x
  }
  
  
  # Function to set (cache) the inverse matrix
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  
  # Function to get the cached inverse matrix
  getinverse <- function() {
    inv
  }
  
  
  # Return a list of functions (methods) for this special matrix object
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix
## This function calculates the inverse of the special "matrix" created with 
## makeCacheMatrix. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the matrix and sets 
## the value of the inverse in the cache via the setinverse function.
##
## Arguments:
##   x: A special "matrix" object created by makeCacheMatrix
##   ...: Additional arguments passed to the solve() function
##
## Returns:
##   The inverse of the matrix
cacheSolve <- function(x, ...) {
  
  # Attempt to retrieve cached inverse
  inv <- x$getinverse()
  
  
  # If inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not cached, get the matrix
  data <- x$get()
  
  # Calculate the inverse using solve()
  inv <- solve(data, ...)
  
  # Cache the calculated inverse
  x$setinverse(inv)
  
  # Return the inverse
  inv
}