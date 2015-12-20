## Put comments here that give an overall description of what your
## functions do

## Examples:
## x <- matrix(data = c(1,2,3,6,5,4,3,6,5),ncol=3,nrow=3)
## li <- makeCacheMatrix(x)
## cacheSolve(li)
## It will return the inverse of x. Next try cacheSolve again.
## cacheSolve(li)
## It will print 'getting cached data'


## Write a short comment describing this function
## This function creates a data structure that stores functions
## handling the matrix and the calculation of inverse.
## See the example above for the use of this function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function uses the list structure created by the above function
## and returns the inverse of the matrix.
## See the example above for the use of this function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
