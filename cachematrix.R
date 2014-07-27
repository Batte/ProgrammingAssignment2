## makeCacheMatrix creates 4 atomic functions
## set the value of the supplied matrix, get the matrix
## return the inverse of the matrix, from cache if possible
## otherwise calculate the inverse and cache the results

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(t) {
    x <<- t
    s <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) s <<- solve
  getInverse <- function() s
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve returns the inverse of the original matrix x
## from cache if possible

cacheSolve <- function(x, ...) {
  s <- x$getInverse()
  if(!is.null(s)) {
    message("inverse was already calculated and cached")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setInverse(s)
  ## Return a matrix s that is the inverse of 'x'
  s
}
