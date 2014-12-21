## These functions calculates the inverse of the input matrix if there
## isn't any cahed data for it.

## The first function will create a list containing input matrix (x)
## and cached inverse of it (I), which is initialized as NULL. 

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y = matrix()) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse=matrix()) I <<- Inverse
        getInverse <- function() I
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The second function checks if there is the cached data for the matrix 
## inverse. If it exists the function will return cached matrix else it will
## calculate the matrix inverse and store it to the cache.

cacheSolve <- function(x, ...) {
        I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        I
}
