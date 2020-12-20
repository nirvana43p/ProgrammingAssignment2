## Put comments here that give an overall description of what your
## functions do



# This function creates a special
# matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Defining the inverse of the matrix at the beginning
  inverse <- NULL
  # Defining the set function to set the cache
  set_x <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # Defining the get function to get the x matrix
  get_x <- function() x
  # Defining the set_inverse function to set the inverse of x
  set_inverse <- function(newInverse) inverse <<- newInverse
  # Defining the get_inverse function to get the inverse of x
  get_inverse <- function() inverse
  
  # Returning a list of functions created 
  list(set_x = set_x, get_x = get_x,
       set_inverse = set_inverse, get_inverse = get_inverse)
  
}


# This function computes the inverse of the special
# "matrix" returned by makeCacheMatrix above.


cacheSolve <- function(x, ...) {
  # Getting the inverse of x object
  inverse <- x$get_inverse()
  
  # Check if the inverse has already computed 
  if (!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  
  # Getting the x matrix
  x_matrix <- x$get_x()
  # Computing the inverse of x
  inverse <- solve(x_matrix, ...)
  print(inverse)
  # Setting the new inverse in the cache
  x$set_inverse(inverse)
  # Returning the new inverse
      
} 





