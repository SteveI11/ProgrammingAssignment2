## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  #set the value of the matrix
   m <- matrix()
    set <- function(y) {
      x <<- y
      m <<- NULL
  }
  #get the value of the matrix
   get <- function() x
  #set the inverse of the matrix
    setinverse <- function(inverse) m <<- inverse
  #get the value of the inverse
   getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse is already calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #check o see if inverse is cached  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #calculate inverse if not cached
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}
