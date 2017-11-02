## Put comments here that give an overall description of what your
## functions do
###Function "makeCacheMatrix" sets up a number of functions within itself to apply to the 
###values later entered to acquire the relevant sole outputs and create an data element into 
###which to store the cache.  "cacheSolve" compeltes the "makeCacheMatrix" by then running a 
###matrix through each function to test if it has already been entered and cached, returning previously 
###cached items faster. 

## Write a short comment describing this function

#Creates relevant functions to check data and create cache locations.
#To use:
# - Call matrix into an object e.g. m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# - Call makeCacheMatrix on the above object to a new object 
#   e.g. mymatrixobject <- makeCacheMatrix(m1)
#   This stores the relevant information (functions and data/cache locations) to allow funciton 2 to work correctly.

makeCacheMatrix <- function(x ) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
# This function now tests whether a matrix has already been called through the functions above and an
#inverted matrix saved in the cache. If it has already been saved the matrix will be returned, below the message 
# "getting cached data".  It it has not already been cached, the computation is performed and output.
# To use:
# - Call cacheSolve on the output created above (in example given above this is the object mymatrixdata)
#   e.g. cacheSolve(mymatrixdata)


cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s    
    ## Return a matrix that is the inverse of 'x'
}
