## Put comments here that give an overall description of what your
## functions do
##

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize local inverse variable
        xInverse <- NULL
        
        ## Methods
        setMatrix <- function(newMatrix) {
                # copy new value and clear inverse
                x <<- newMatrix
                xInverse <<- NULL
        }
      
        getMatrix <- function() x
        
        setInverse <- function(inverse) xInverse <<- inverse
        
        getInverse <- function() xInverse
        
        ## Return the list object
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixInverse <- x$getInverse()
        if(!is.null(matrixInverse)) {
                message("getting cached data")
                return(matrixInverse)
        }
        theMatrix <- x$getMatrix()
        matrixInverse <- solve(theMatrix)
        x$setInverse (matrixInverse)
        matrixInverse
}
