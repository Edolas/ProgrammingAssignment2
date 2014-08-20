## If a matrix is used in the makeCacheMatrix function the resulting list can be used in the cacheSolve function;
## the cacheSolve then either retreives the stored inverse of the matrix or calculates and stores it.


## The makeCacheMatrix creates a list for a given matrix that stores: set, get, setinv and getinv and creates a inv = NULL

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function uses the makeCacheMatrix list to return the inverse of the matrix
## The returned value is either collected from the "inv" and prints "getting cached data" or calculated if "inv" = NULL

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}