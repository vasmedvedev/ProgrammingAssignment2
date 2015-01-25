## Functions perform calculations of matrix inverse with ability to cache results and 
## checking if matrix is inversible

## Function makeCacheMatrix accept matrix as an argument
## and constructs expose list of methods to get and set cached value

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
    
## Accepts matrix constructed by makeCacheMatrix, calculates and returns inverse matrix,
## of returns cached result, also check if matrix is inversible

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    if (det(data) == 0) {
        message("determinant equals zero, the matrix is singular")
        return(NA)
    }
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
