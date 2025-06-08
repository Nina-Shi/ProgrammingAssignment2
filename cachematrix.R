## These functions create a special "matrix" object that can cache its inverse,
## and compute/retrieve the inverse of this matrix to avoid redundant computation.

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to:
##  - set the value of the matrix
##  - get the value of the matrix
##  - set the value of the inverse
##  - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cached inverse is retrieved. Otherwise, it computes the inverse,
## stores it in the cache, and returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}