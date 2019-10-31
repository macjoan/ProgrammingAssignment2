## They are two functions, one creates a special matrix 
## and the second calculates the inverse
## or get it from cache

## Tis function set and get the value of the matrix
## and set and get the value of the inverse matrix

makeCacheMatrix <- function(X = matrix()) {
        AI <- NULL
        
        setMatrix <- function(Y) {
                X <<- Y
                AI <<- NULL
        }
        getMatrix <- function() X
        setInverse <- function(inverse) AI <<- inverse
        getInverse <- function() AI
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)

}


## This function return a inverse of a matrix or take the value of
## inverse matrix from the cache.

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'
        AI <- X$getInverse()
        if(!is.null(AI)) {
                message("Get cache inverse matrix")
                return(AI)
        }
        Mdata <- X$getMatrix()
        AI <- solve(Mdata, ...)
        X$setInver(AI)
        AI
}
