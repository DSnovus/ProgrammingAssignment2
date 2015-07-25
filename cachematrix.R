## makeCacheMatrix creates a list which contains the value of a matrix and the value of inverse of the matrix.
## cacheMatrix calculates the inverse of the matrix created with the above function. It first checks if the inverse
## has already been calculated. If it has, the function returns the inverse from the cache and skips the computation.
## Otherwise, it calculate the inverse and sets the value of the inverse matrix witht he setsolve function. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve checks if the  inverse has alreayd been calculated. If it has, the function returns the inverse from the cache and skips the computation.
## Otherwise it calculate the inverse and sets the value of the inverse matrix witht he setsolve function. 

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
  ## Return a matrix that is the inverse of 'x'
}
