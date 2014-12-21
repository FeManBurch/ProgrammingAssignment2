## makeCacheMatrix creates a matrix object with an iniital value equal to the
## passed value, and a cached copy (originally set to NULL) is overwritten
## with the initial value if different from the initial value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns the cached value of the inverse of x if x is a new
## matrix, otherwise it calculates, caches, and returns the inverse of x

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

testm <- matrix(1:9,3,3)
testn <- matrix(1:25,5,5)

BigMat <- makeCacheMatrix(testm)
BiggerMatrix <- makeCacheMatrix(testn)

cacheSolve(BigMat)
