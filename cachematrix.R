## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.

## Assume that the matrix supplied is always invertible.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  InvMat <- NULL
  set <- function(y) {
    x <<- y
    InvMat <<- NULL
  }
  get <- function() x
  setInv <- function(solve) InvMat <<- solve
  getInv <- function () InvMat
  list (set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve:  This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  InvMat <- x$getInv()
  if (!is.null(InvMat)) {
    message("Getting cached matrix")
    return(InvMat)
  }
  data <- x$get()
  InvMat <- solve(data, ...)
  x$setInv(InvMat)
  InvMat
}
