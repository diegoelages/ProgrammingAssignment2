## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will
##             not discuss here). Your assignment is to write a pair of functions that
## cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  xi <- matrix()   ## Create a empty matrix for matrix inversion
  set <- function(y){
    x <<- y
    xi <<- matrix()
  }
  get <- function() x
  setmatrixinversion <- function(mi) xi <<- mi
  getmatrixinversion <- function() xi
  list(set = set, get = get,
       setmatrixinversion = setmatrixinversion,
       getmatrixinversion = getmatrixinversion)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  xi <- x$getmatrixinversion()
  if (!is.na(xi)){
    message("getting cached data")
    return(xi)
  }
  m <- x$get()
  xi <- solve(m)
  x$setmatrixinversion(xi)
  xi                        ## Return a matrix that is the inverse of 'x'
}
