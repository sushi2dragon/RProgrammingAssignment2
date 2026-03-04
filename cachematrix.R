## These functions provide a mechanism to cache the inverse of a matrix.
## Storing the result of the inversion avoids the computational overhead of 
## recalculating it repeatedly if the matrix remains unchanged.

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions used to set and get the matrix, 
## as well as set and get the cached inverse value.

makeCacheMatrix <- function(x = matrix()) {
  # 'inv' stores the cached inverse; initialized to NULL
  inv <- NULL
  
  # Function to define the matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL # Resets the cache if the matrix is updated
  }
  
  # Function to retrieve the matrix value
  get <- function() x
  
  # Function to assign the inverse value in the parent environment
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to retrieve the cached inverse value
  getInverse <- function() inv
  
  # Returns a list containing the functions defined above
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse is already calculated 
## (and the matrix is unchanged), the function retrieves the 
## inverse directly from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it with a message
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If no cache exists, get the matrix, calculate the inverse, and store it
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  # Return the calculated inverse
  inv
}