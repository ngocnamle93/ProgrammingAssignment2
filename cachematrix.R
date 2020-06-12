## Put comments here that give an overall description of what your
## functions do

## This function can create a matrix that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinv <- function(solMatrix) inver <<- solMatrix
        getinv <- function() inver
        list(set = set, get=get, setinv =setiv, getinv = getinv)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinv()
        if(!is.null(inver)){
                message("getting cached data")
                return(inver)
        }
        dat <- x$get()
        inver <- solve(dat)
        x$setinv(inver)
        inver
}
