## The functions below compute the inverse of a matrix and cache the result. 
## If the matrix has not changed, subsequent calls to the function will access the cached result
## and avoiding continuous re-computation.


## the makeCacheMatrix function takes a matrix parameter and returns a list of functions
## to set and get the value of the matrix and get and set the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       getinverse = getinverse,
       setinverse = setinverse)
}


## the cacheSolve function returns the inverse of the makeCache matrix
## it first checks to see if the inverse is already cached and if not it will calculate it
## and set it using the setinverse function.

cacheSolve <- function(x,...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}