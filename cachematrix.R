## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Variable to store the cached inverse

  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset cache when the matrix is updated
  }

  get <- function() x  # Retrieve the matrix

  setInverse <- function(inverse) inv <<- inverse  # Store the inverse in cache

  getInverse <- function() inv  # Retrieve the cached inverse

  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix"
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()

  # If inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }

  # Otherwise, compute the inverse and cache it
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Store the inverse in cache

  inv  # Return the computed inverse
}

# Example Usage:
mat <- matrix(c(1, 2, 3, 4), 2, 2)  # Define a 2x2 matrix
cachedMatrix <- makeCacheMatrix(mat)  # Create the special matrix object

cacheSolve(cachedMatrix)  # Computes and caches the inverse
cacheSolve(cachedMatrix)  # Retrieves cached inverse

cachedMatrix$set(matrix(c(2, 3, 4, 5), 2, 2))  # Update the matrix
cacheSolve(cachedMatrix)  # Computes new inverse

