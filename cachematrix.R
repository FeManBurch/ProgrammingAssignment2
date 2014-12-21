## makeCacheMatrix creates a matrix object that is a list with 4 elements,
## set/get values of the matrix and its inverse

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

## cacheSolve returns the cached value of the inverse of x if x the same
## as the prior function call, otherwise it calculates, caches, and returns 
## the inverse of x

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

## here is what i used to test my code
## testa <- matrix(c(1,0,0,1,1,0,1,1,1),3,3)
## testb <- matrix(c(1,0,0,0,1,1,0,0,1,1,1,0,1,1,1,1),4,4)
## BigMat <- makeCacheMatrix(testa)
## BiggerMat <- makeCacheMatrix(testb)
## cacheSolve(BigMat)
## cacheSolve(BigMat)
## cacheSolve(BiggerMat)

