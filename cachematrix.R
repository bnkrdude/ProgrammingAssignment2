## These two functions will cache the inverse of a matrix and return
## the inverse without recalculating (assuming it has been calculated already)
## saving processing resources and time

## The makeCacheMatrix function creates a list with a function that:
## sets and gets the value fo the matrix and sets and gets the value of
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
  }
        get <- function() x
        
        setinv <- function(inverse) inv <<- inverse
        
        getinv <- function() inv
  
  list(set = set, get = get,    
       setinv = setinv,
       getinv = getinv)

}  # End makeCacheMatrix


## Returns the inverse of the matrix.  If the matrix has already been computed and stored
## in cache, then it does not calculate the matrix, but pulls it from cache.
## If the matrix is not in cache, then the function calculates it and puts it in cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        
        if(!is.null(inv)) {
           message("getting cached data")
           return(inv)
  
        }  # end if
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        
}  # End of cacheSolve
