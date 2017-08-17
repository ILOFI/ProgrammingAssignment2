## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache 
## its reverse. 
## It returns a list with function to:
##  - set the value of matrix
##  - get the value of matrix
##  - set the value of the reverse
##  - get the value of the reverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setreverse <- function(reverse) m <<- reverse
    getreverse <- function() m
    list(set = set, get = get,
         setreverse = setreverse,
         getreverse = getreverse)
}


## Write a short comment describing this function

## This This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getreverse()
    if (!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setreverse(m)
    m
}
