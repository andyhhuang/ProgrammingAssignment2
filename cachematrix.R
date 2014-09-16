## Create an object that can store a matrix as well
## as caching the inverse of the matrix inside the
## object to save future calculation time

## Create a special object that points to an external
## matrix object. There are also two getter and setter
## pairs for getting and setting the external matrix
## value as well as the internal inverse matrix value.
##
## x : the external matrix
makeCacheMatrix <- function(x = matrix()) {
    ## Declare and initiate the inverse matrix variable
    i <- NULL
    
    ## Points to the matrix data. It's important to
    ## clear the cached inverse at this point since
    ## it's no longer valid.
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Returns the external matrix data
    get <- function() x
    
    ## Stores the inverse
    setinverse <- function(inverse) i <<- inverse
    
    ## Returns the inverse
    getinverse <- function() i
    
    ## Public functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return a cached matrix that is the inverse of 'x'.
## If that doesn't exist, calculate the inverse matrix
## by calling the solve function on x, cache it, and
## return it
##
## x : the special object that caches the inverse
cacheSolve <- function(x, ...) {
    ## Get the inverse matrix
    i <- x$getinverse()
    
    ## If such matrix exists, return it
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Otherwise, get the stored matrix data
    data <- x$get()
    
    ## Calculate its inverse
    i <- solve(data, ...)
    
    ## Cache the inverse
    x$setinverse(i)
    
    ## Return a matrix that is the inverse of 'x'
    i
}
