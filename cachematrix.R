## These functions enable computationally-expensive matrix inversion operations
##  to be cached, so that a given matrix is only inverted once, and on future
##  requests, the stored inverted matrix is returned.
##
## An example:
##  a <- matrix(c(10,5,2,8),nrow=2)
##  a2 <- makeCacheMatrix(a)
##  cacheSolve(a2) ## Calculates and returns inverse
##  cacheSolve(a2) ## Now just returns inverse

## makeCacheMatrix returns a function which has four functions:
##   - set(matrix)
##   - get()
##   - setinverse(matrix)
##   - getinverse()
##  as well as two properties:
##   - x [the matrix]
##   - inverse [the cached inverted matrix]
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(newmatrix) {
    x <<- newmatrix
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverted) {
    inverse <<- inverted
  }
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = cacgetinverse)
}

## cacheSolve is a function which returns on request 
##  a matrix which is the inverse of the one provided.
##  [It requires an instantiation of makeCacheMatrix
##  as its input]
cacheSolve <- function(x, ...) {
  currentinverse <- x$getinverse()
  if(!is.null(currentinverse)) {
    message("Getting cache...")
    return(currentinverse)
  }
  matrix <- x$get()
  inverted <- solve(matrix)
  x$setinverse(inverted)
  return(inverted)
}
