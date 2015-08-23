## This code is an adaptation of the example code "Caching the Mean of a Vector"
## for "Caching the Inverse of a Matrix"

## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse. It works together
## with the function cacheSolve to obtain the inverse of invetible matrices
## Instructions:
## An iversible matrix like "rbind(c(1, -1/4), c(-1/4, 1))" has to be entered to the function 
## The result is asigned to an object like "a" to be used later by the function cacheSolve
##
##     example:                 a <- makeCacheMatrix (rbind(c(1, -1/4), c(-1/4, 1)))

makeCacheMatrix <- function(x = matrix()) {            ## vector replaced by matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse   ## setmean repleaced by setinverse
        getinverse <- function() m                      ## getmean replaced by getinverse                 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
## is used entering the object "a" where the special "matrix" was stored.
## the secong time the funtion is run it gets the result from cache
## example                      cacheSolve(a)

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)          ## mean function replaced by solve function
        x$setinverse(m)
        m
        ## m is a matrix that is the inverse of 'x'
}
