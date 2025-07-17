## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    
    # Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset inverse when matrix changes
    }
    
    # Get the matrix
    get <- function() x
    
    # Set the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # Get the inverse
    getinverse <- function() inv
    
    # Return list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Computes the inverse of the special "matrix" or retrieves from cache
cacheSolve <- function(x, ...) {
    # Check if inverse is already cached
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    # If not cached, compute the inverse
    data <- x$get()
    inv <- solve(data, ...)  # Compute matrix inverse
    
    # Cache the inverse
    x$setinverse(inv)
    
    # Return the inverse
    inv
}