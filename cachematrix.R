makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that cache its inverse.
  ## return a list containing functions to set the matrix, get the matrix, 
  ## set the inverse, get the inverse.
  ## The list is used as input to next function (cacheSolve)
  inv <- NULL
  set <- function(y) {
    ## to assign a value to an object in an environment different from the current
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
cacheSolve <- function(x, ...) {
  ## x - output of makeCacheMatrix
  ## return inverse of the original matrix
  inv <- x$getinv()
  ## if the inverse has alredy been calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## if not, calculates the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}