## Contains 2 functions for working with the inverse of a matrix:
## 1. makeCacheMatrix - creates a special "matrix" object that can calculate its inverse
## 2. cacheSolve - calculates, caches and returns the inverse of this "matrix".  
##    Calculation is skipped if the inverse has already been cached (and the matrix 
##    has not changed).
## NOTE: The code in these functions is based on the sample code provided in the assignment.

## This function creates a special "matrix" object that can calculate its inverse and cache
## the result.  It returns a list containing the following functions:
## - set() - sets the value of the matrix
## - get() - gets the value of the matrix
## - setinverse() - sets the value of the inverse of the matrix
## - getinverse() - gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## inv will hold the inverse.  initialize it to NULL.
    inv <- NULL
    ## Create set function which will set the value of the matrix (x) to the new matrix object
    ## (newMatrix)
    set <- function(newMatrix) {
        x <<- newMatrix
        inv <<- NULL
    }
    # Create get function which will return the matrix object
    get <- function() x
    ## Create the setinverse function to set the value of the inverse (inv) to the value passed
    ## to the function (newInv).  
    setinverse <- function(newInv) inv <<- newInv
    ## Create the getinverse function to return the value of the inverse
    getinverse <- function() inv
    ## Return the list of functions created above
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes and returns the inverse of the special "matrix" returned 
## by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the function retrieves the inverse from the cache and returns that value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## check to see if the inverse is already stored in the cache
    myInverse <- x$getinverse()
    if (!is.null(myInverse)) {
        ## the inverse is cached, so return it
        message("Getting inverse from the cache")
        return(myInverse)
    }
    ## the inverse is not cached, so calculate, store and return it
    
    ## get the matrix data
    mData <- x$get()
    ## calcuate the inverse of the matrix
    myInverse <- solve(mData, ...)
    ## store the inverse
    x$setinverse(myInverse)
    ## return the inverse
    myInverse
    
}
