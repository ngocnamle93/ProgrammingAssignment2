## These two equations compute the matrix's inverse.

## This function can create a matrix that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # x is a square invertible matrix
        inver <- NULL
        set <- function(y) { #set a list
                x <<- y
                inver <<- NULL
        }
        get <- function() x # get a list
        setinv <- function(solMatrix) inver <<- solMatrix # set the inverse
        # Use << to assign a value to an object
        getinv <- function() inver # get the inverse
        list(set = set, get=get, setinv =setiv, getinv = getinv)
}

## Computes the inverse of the special matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # if the inverse has already been calculated
        # take it from the cache
        inver <- x$getinv()
        if(!is.null(inver)){
                message("getting cached data")
                return(inver)
        }
        # If not, calculate:
        dat <- x$get()
        inver <- solve(dat)
        x$setinv(inver) # Set the inverse value
        inver
}
