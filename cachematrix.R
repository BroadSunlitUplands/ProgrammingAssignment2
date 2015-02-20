# The two functions below demonstrate how R uses lexical
# scoping.

# Function: makeCacheMatrix 
# Purpose:  To create a special "matrix" object, which sets and 
#           retrieves the value of a matrix, plus sets or 
#           retrieves the value of a matrix's inverse. 
# Inputs:   A matrix
# Outputs:  A new, reset, or cached matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) inv <<- solve
  get_inv <- function() inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

# Function: cacheSolve 
# Purpose:  To calculate the inverse of the "special" matrix 
#           returned by the function makeCacheMatrix, or return
#           the cached value of the "special" matrix.
# Inputs:   A matrix
# Outputs:  A matrix's inverse, either from a cache or 
#           calculated within the function.

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}
