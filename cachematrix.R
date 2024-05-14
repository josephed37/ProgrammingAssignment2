## Put comments here that give an overall description of what your
## functions do

# Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse matrix as NULL
  inv <- NULL
  
  # Setter function to set the matrix and invalidate the cache
  set <- function(mat) {
    x <<- mat
    inv <<- NULL  # Invalidate cache
  }
  
  # Getter function to get the matrix
  get <- function() {
    x
  }
  
  # Setter function to set the inverse and cache it
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Getter function to get the inverse
  getInverse <- function() {
    inv
  }
  
  # Return a list containing the setter and getter functions
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the matrix
cacheSolve <- function(cacheMatrix, ...) {
  # Retrieve the cached inverse if available
  inv <- cacheMatrix$getInverse()
  
  # If the inverse is cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not cached, compute the inverse using solve() function
  mat <- cacheMatrix$get()
  inv <- solve(mat, ...)
  
  # Cache the computed inverse
  cacheMatrix$setInverse(inv)
  
  # Return the computed inverse
  inv
}
