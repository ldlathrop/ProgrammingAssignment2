## Functions to cache an inverted matrix and then return
##the inversed cache, if available

## x is a square invertible matrix


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
            ## Creates a special "matrix" object that can cache its inverse
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get, 
           setinv = setinv,
           getinv = getinv)
      ## Gets the matrix
      ## Sets the inverse
      ## Gets the inverse

}


## Returns the cached and inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            ## If the inverse has already been calculated
            ## return the inverse
            message("getting cached data")
            return(inv)
      }
      ## If not already calculated, get the matrix,
      ## set and return the inverse.
      matrix <- x$get()
      inv <- solve(matrix, ...)
      x$setinv(inv)
      inv
}
