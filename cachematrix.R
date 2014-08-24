## Put comments here that give an overall description of what your
## functions do
## Useful guide: https://class.coursera.org/rprog-006/forum/thread?thread_id=204


## Function contains a list of subfunctions
## that can be subsequently called. Also
## serve as a cache where the inverse matrix
## of interest is stored as 'm'

makeCacheMatrix <- function(x = matrix()) {  
  m <- NULL ## inverse matrix of our interest default to NULL on initialization
            ## This is also the value that is stored as the cache
  
  # Redefine the input 'x' when called
  # and also redefaults the cache 'm' to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Returns the value of the input 'x'
  get <- function() x
  
  # Stores the value to the cache 'm' when called
  setinverse <- function(mean) m <<- mean 
  
  # Return the cache value
  getinverse <- function() m
  
  # List of all the functions that can be called
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Check if the inverse of the matrix we want has
## already been calculated and return the pre-computed
## value if so. Otherwise, calculate it and return

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  # Check if the value has already been calculated
  # and is stored in cache. If so, just retrieve
  # value from cache and return it.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Only performed if the previous 'if' test fails.
  # This performs the calculation of the inverse
  data <- x$get()
  m <- solve(data, ...)
  
  # Stores the computed inverse to the cache
  # so it can retrieved later if needed
  x$setinverse(m)
  m
}