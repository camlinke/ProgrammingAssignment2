## Put comments here that give an overall description of what your
## functions do

## This library is creates a matrix that can cache it's inverse
## and gives computes the inverse of the created matrix

## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ## Set and cache the inverse of the matrix using the solve function
  setinverse <- function(solve) m <<- inverse
  getinverse <- function() m
  ## Create a list containing a function to set the value of the matrix, get the value of the matrix,
  ## set the value of the inverse, and get the value of the inverse.
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  ## check to see if there is already a cached inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If there is no cached inverse calculate the inverse, cache it and return it
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
