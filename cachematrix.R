## cacheSolve solves a matrix (computes the inverse of a matrix)
## using a cacheable matrix created by makeCacheMatrix

## makeCacheMatrix - creates a cacheable matrix, which caches inverse of 
## self inside it, designed to recalculate inverse again after it gets modified
## use the cached inverse otherwise

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve solves a matrix (computes the inverse of a matrix)
## using a cacheable matrix created by makeCacheMatrix

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
