## Caching the Inverse of a Matrix - Week 3 assignment

## This function creates a special "matrix" object that can cache its inverse
## It return a list containing functions:
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
      r <- NULL
      set <- function(y) {
            x <<- y
            r <<- NULL
      }
      get <- function() x
      setmtx <- function(solve) r <<- solve
      getmtx <- function() r
      list(set = set, get = get,
           setmtx = setmtx,
           getmtx = getmtx)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
      r <- x$getmtx()
      if(!is.null(r)) {
            message("getting cached data")
            return(r)
      }
      data <- x$get()
      r <- solve(data,...)
      x$setmtx(r)
      r
}
