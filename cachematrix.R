## These functions calculates the inverse of the input matrix if there
## isn't any cahed data for it.

## The first function will create a list containing input matrix (x)
## and cached inverse of it (I), which is initialized as NULL. 

makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x
        set_Inv <- function(inverse) inv_m <<- inverse
        get_Inv <- function() inv_m
        list(set = set, get = get,
             set_Inv = set_Inv,
             get_Inv = get_Inv)
}


## The second function checks if there is the cached data for the matrix 
## inverse. If it exists the function will return cached matrix else it will
## calculate the matrix inverse and store it to the cache.

cacheSolve <- function(x, ...) {
        inv_m <- x$get_Inv()
        if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
        }
        data <- x$get()
        inv_m <- solve(data, ...)
        x$set_Inv(inv_m)
        inv_m
}
