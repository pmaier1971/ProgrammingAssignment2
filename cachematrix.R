
## The following two functions cache the inverse of a matrix.
##     makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##    cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## The inverse of the matrix is calculated using the solve function.


##  This function creates a special "matrix" object that can cache its inverse.
##  We also store the matrix on which the inverse was calculated in 'setMatrix'
makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 set <- function(y) {
   x <<- y
   m <<- NULL
 }
 get <- function() x
 setSolve <- function(solve) m <<- solve
 getSolve <- function() m
 # Next two lines store / retrieve the matrix for which is inverse is (was) calculated
 setMatrix <- function(cached) cachedMatrix <<- cached
 getMatrix <- function() cachedMatrix
 list(set = set, get = get,
      setMatrix = setMatrix, getMatrix = getMatrix,
      setSolve = setSolve,
      getSolve = getSolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## retrieves the inverse from the cache.
## Note: The function assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
 data <- x$get()
 x$setMatrix(data)

 # Check whether we previously calculated the inverse
 if(!is.null(x$getMatrix())) {
   # Check whether the matrix has changed
   if (identical(x$get(), x$getMatrix())) {
     m <- x$getSolve()
     if(!is.null(m)) {
       message("Getting cached data")
       return(m)
     }
   }
 }
 m <- solve(data, ...)
 x$setSolve(m)
 m
}

