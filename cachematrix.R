## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  makeVector <- function(x = numeric()) {
    
  #set the value of the vector
   m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
  }
  #get the value of the vector
   get <- function() x
  #set the value of the mean
    setmean <- function(mean) m <<- mean
  #get the value of the mean
   getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
  }  
  
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse is already calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  #check o see if mean is cached  
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #calculate mean if not cached
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}


}
