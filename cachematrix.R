## These functions provide a way to cache the inverse of a matrix, to prevent recalculating
## it if the matrix hasn't changed

## Creates a matrix that caches its inverse. A list containing functions
## to get\set the value of the matrix, and get\set the value of the matrix inverse
## is returned
makeCacheMatrix <- function(x = matrix())
{
    #Reset the inverse
    inverse <- NULL
    
    #Function used to set the value of the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    #Function used to get the value of the matrix
    get <- function() x
    
    #Function used to set cache the inverse
    setInverse <- function(inv) inverse <<- inv
    
    #Function used to get the cached inverse
    getInverse <- function() inverse
    
    #List holding it all together
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Solves the inverse of a matrix. First checks if
## the result has not been cahced before, if so, the cached
## value is used
cacheSolve <- function(x, ...)
{
    #Get the inverse first
    inverse <- x$getInverse()
    #and check if it has been cached already
    if(!is.null(inverse))
    {
        #Yes. Use the cached value
        message("getting cached inverse")
        return(inverse)
    }
    #Inverse not cached, so calculate it
    data <- x$get()
    inverse <- solve(data)
    #Cache the inverse for later use
    x$setInverse(inverse)
    inverse
}
