## Functions to calculate the memoized (https://en.wikipedia.org/wiki/Memoization)
## and lazily evaluated (https://en.wikipedia.org/wiki/Lazy_evaluation) inverse 
## of a square and invertable matrix.

## Function that takes a matrix and returns a "cache matrix": a new object 
## that allows retrieving and storing the original matrix and its inverse.
## Intended to be used with the cacheSolve() function.

makeCacheMatrix <- function(original_matrix = matrix()) {
  cached_inverse <- NULL
  
  set <- function(new_matrix) {
    original_matrix <<- new_matrix
    cached_inverse <<- NULL
  }
  
  get <- function() original_matrix
  
  set_inverse <- function(inverse_matrix) cached_inverse <<- inverse_matrix
  get_inverse <- function() cached_inverse
  
  list(set = set, 
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse
  )
}


## Function that accepts a cache matrix (object returned by the makeCacheMatrix()
## function) and returns the inverse matrix. The inverse matrix is cached, so
## subsequent calls are not faster.
## Assumes matrices are square and invertable.
## Delegates to the solve() function for the inverse matrix calculation.

cacheSolve <- function(cache_matrix, ...) {
  ## Return cached inverse matrix if it has already been calculated
  cached_inverse <- cache_matrix$get_inverse()
  
  if(!is.null(cached_inverse)) {
    return(cached_inverse)
  }
  
  ## The inverse has not already been calculated, so do so and store
  ## it on the cache matrix for later
  original_matrix <- cache_matrix$get()
  
  calculated_inverse <- solve(original_matrix, ...)
  
  cache_matrix$set_inverse(calculated_inverse)
  
  calculated_inverse
}
