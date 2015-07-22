## A pair of functions that enable the 
## inverse of a matrix to be non-redundantly computed via storage as a cache variable

## A constructor function that, given a matrix as input, initializes the inverse 
## of the matrix to NULL and creates a list object containing 3 elements: 1) a function
## to return the matrix, 2) a function to store the inverse of the matrix to cache
## ,and 3) a function that returns the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}


## Given the constructed list from makeCacheMatrix, checks to see whether the inverse has
## already been computed and stored in cache. If so, then returns inverse and exits 
## Otherwise, computes the inverse of the matrix
## stores the value to the cached variable, and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
