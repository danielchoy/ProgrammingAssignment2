## Functions Objective:
## Reduce computation by caching the inverse of a matrix instead of repeatedly computing it

## The first function, makeCacheMatrix creates a special "matrix" object "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inversematrix) inv_mat <<- inversematrix
  getinversematrix <- function() inv_mat
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
  
}


## The second function, cacheSolve returns an inverse matrixcreated with the makeCacheMatrix function
## If the inverse matrix has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getinversematrix()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data)
  x$setinversematrix(inv_mat)
  inv_mat
  
}
