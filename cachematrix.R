## Put comments here that give an overall description of what your

## functions do



## create CacheMatrix by passing initial matrix, includes functions set and get


makeCacheMatrix <- function(x = matrix()) {
  
  inv_x<- NULL
  set <- function(y) {
    x <<- y
    inv_x<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse_x) inv_x <<- inverse_x
  get_inverse <- function() inv_x
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}





## compute inverse for the matrix 



cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$get_inverse()
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data, ...)
  x$set_inverse(inv_x)
  inv_x
  
}