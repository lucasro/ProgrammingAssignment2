## Matrix inversion can be a costly computation. Therefore, it could be useful to
## cache the inverse of a matrix rather than compute it over and over again. To do this,
## one can make use of the following two functions: makeCacheMatrix() and cacheSolve().
## Note that these functions assume the input matrix to be invertible. 
## An example is provided below the function definitions.

## The makeCacheMatrix() function creates a list containing four elements: (1) a function
## to set the value of the matrix, (2) a function to get the value of the matrix, (3) a 
## function to set the value of the inverse of the matrix, and (4) a function to get the
## value of the inverse of the matrix. The resulting output of this function should be 
## used as input to the cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(calculated_inverse) inverse <<- calculated_inverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)      ## Return the list of functions.
}


## The cacheSolve() function returns the inverse of the matrix. First, it checks if
## the inverse has already been computed before. If so, it gets the cached inverse and 
## returns it. If not, the function calculates the inverse, sets the value in the cache 
## and returns it.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()                   
    if(!is.null(inverse)) {                 # Check if the inverse was catched.
        message("getting cached data")      # If so, return chached inverse without
        return(inverse)                     # executing the subsequent code.
    }
    data <- x$get()                         # Otherwise: get the data,
    inverse <- solve(data, ...)             # compute the inverse,
    x$setinverse(inverse)                   # catch the inverse,
    inverse                                 # and return the inverse.
}

## Example to illustrate the functions:
##
## x <- matrix(c(-0.5,1,1,-0.5), nrow = 2)
## res <- makeCacheMatrix(x)
## cacheSolve(res)
##
## There is no cached matrix in the first run. However, when one executes the function 
## a second time, the inverse will be retrieved from the cache:
##
## cacheSolve(res)