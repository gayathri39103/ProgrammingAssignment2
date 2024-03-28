## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix function creates a special matrix object that can cache its inverse. It returns a list of functions including set, get, setInverse, and getInverse.
## cacheSolve function computes the inverse of the matrix and caches it if not already cached. It returns the inverse and retrieves the cached inverse if available.


# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse matrix cache
  inverse <- NULL
  
  # Function to set the matrix
  set <- function(matrix) {
    x <<- matrix
    # Reset inverse cache when matrix changes
    inverse <<- NULL
  }
  
  # Function to get the matrix
  get <- function() {
    x
  }
  
  # Function to set the inverse of the matrix
  setInverse <- function(inv) {
    inverse <<- inv
  }
  
  # Function to get the inverse of the matrix
  getInverse <- function() {
    inverse
  }
  
  # Return a list of functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" and cache it
cacheSolve <- function(x, ...) {
  # Get the cached inverse if available
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Retrieving cached inverse.")
    return(inv)
  }
  
  # Compute the inverse using solve function
  mat <- x$get()
  inv <- solve(mat, ...)
  
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the computed inverse
  inv
}
