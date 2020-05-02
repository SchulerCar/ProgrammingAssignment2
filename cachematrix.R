## This file contains two functions
##
## makeCacheMatrix <- function(nm= matrix())
##    Creates a special "matrix" object with the following methods
##      setMatrix(nm):  sets the value of the matrix to nm
##      getMatrix():    gets the value of the matrix
##      setInverse(mi): sets the value of the matrix inverse to mi
##      getInverse():   gets the value of the matrix inverse
##
##
## cachesSolve(specialMatrix)
##    Calculates the inverse of the special "matrix" created with the above
##    function. 
##     - However, it first checks to see if the inverse has already been 
##       calculated. If so, it gets the inverse from the cache and skips the 
##       computation. 
##     - Otherwise, it calculates the inverse of the data and sets the value of the 
##       mean in the cache via the setmean function.


## This is the special "matrix" constructor

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


## This is the function that calculates the inverse for the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixInverse <- x$getInverse()
        if(!is.null(matrixInverse)) {
                message("getting cached data")
                return(matrixInverse)
        }
        theMatrix <- x$getMatrix()
        matrixInverse <- solve(theMatrix, ...)
        x$setInverse (matrixInverse)
        matrixInverse
}
