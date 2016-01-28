## These functions compute the inverse of a matrix.  If the matrix
## for which the inverse needs to be computed was just computed 
## previously, the functions pull the cached inverse instead of 
## re-computing the inverse.

## The makeCacheMatrix function creates a list of 4 functions which are
## used subsequently by the cacheSolve function or used to set new
## matrices for inverse computation.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y 
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) i <<- inv
        getInverse <- function() i 
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


## The cacheSolve function looks to see if the inverse was just computed
## previously and if it was, returns the prior inverse matrix.  If it 
## was not the prior inverse calculation, it re-calculates the inverse.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data") 
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
        ## i the the matrix than is the inverse of 'x'
}
