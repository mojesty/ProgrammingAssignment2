## Put comments here that give an overall description of what your
## functions do

## This function creates special object that can be treated like "Cached matrix"
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<-y
        ##if the value has changed, we will need to compute inverse matrix again
        inv <<-NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv   
    ## return value is list of 4 functions 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    ## Returns cached matrix that is the inverse of 'x'
    inv <- x$getinverse()
    ##checking whether the value of the inverse exists
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    ##updating value
    x$setinverse(inv)
    inv
}
