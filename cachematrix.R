## These functions work together to create a cache for inverses to avoid
## needing to calculate the inverse more than once. cacheSolve takes
## the list created by makeCacheMatrix and uses it to check for and obtain
## already created inverses and create and store newly created inverses. It
## uses values of objects within the environment of makeCacheMatrix to do this.
## Matrixes are assumed to be invertible.

## makeCacheMatrix creates a list of four functions (set, get, setinverse,
## and getinverse) and assigns values to m, x, and y within the environment
## of these functions, so that the functions and values can be used within
## the cacheSolve function:
##   set assigns to x in the global environment the value assigned to y.
##   get takes the value of x for use in cacheSolve.
##   setinverse assigns to me the value of inverse from cacheSolve.
##   getinverse takes the value of m for use in cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes the list from makeCacheMatrix and uses it to retrieve
## an inverse, if one has already been created, or calculate an inverse and
## cache it in makeCacheMatrix, if has not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}