## Caching the Inverse of a Matrix
## A pair of functions, one that creates a list of functions that cache a matrix & its inverse
## and the other that checks the cache before it computes the inverse.

## makeVector returns a list of functions to
## set the value of a matrix
## get the value of a matrix
## set the value of its inverse
## get the value of its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve checks if the inverse of the matrix returned by x is cached, 
## if found it returns the cached value 
## else it calculates the inverse and caches it in x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinverse(i)
  i
}
