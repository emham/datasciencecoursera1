## This script creates one function to store the inverse of a matrix in a cache,
## one to access that inverse if it exists. 

## Creates a list of functions to set and get cached inverse matrix

makeCacheMatrixList <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) I <<- inverse
    getinverse <- function() I
    fnList <- list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
    return(fnList)
}


## Return a matrix that is the inverse of 'x'(matrix as above)
## Takes as input the list of functions created by makeCacheMatrixList
## Will solve and setinverse() if inverse is not yet stored

cacheSolve <- function(xlist, ...) {
    I <- xlist$getinverse()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- xlist$get()
    I <- solve(data, ...)
    xlist$setinverse(I)
    I
}
