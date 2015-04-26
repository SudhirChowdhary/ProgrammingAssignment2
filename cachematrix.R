## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a
## matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).
## Your assignment is to write a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## This Function creates a list containing with following functionality
## a. Set Value of Matrix.
## b. Get Value of Matrix.
## c. Set Value of Inverse Matrix.
## d. Get Value of Inverse Matrix.


makeCacheMatrix <- function(x = matrix()) {
        ## <<- Operator cause a search to made through parent environments for an existing definition of the variable being assigned
        inv_Mat <- NULL ## Initialize the Values to NULL
        set <- function(y) {
                x <<- y  ## Cache the matrix
                inv_Mat <<- NULL  ## set the value of inv_Mat to NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_Mat <<- inverse
        getinverse <- function() inv_Mat
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Key function used is setinverse to ensure the value of inv is set after first  time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_Mat <- x$getinverse()
        ## For the first time, inv_Mat will have NULL value. Second time, the value will be <> NULL
        ## Because it will be assigned by setinverse function from makeCacheMatrix
        if(!is.null(inv_Mat)) {
                message("Values of inv_Mat/ Cached Data from Another Environment")
                return(inv_Mat)
        }
        data <- x$get()
        ## Compute the value of inverse-input-matrix
        inv_Mat <- solve(data)
        ## Sets the value of inv_Mat to <> NULL, so that we get the value from other environment.
        x$setinverse(inv_Mat)
        ## Return the inverse "inv_Mat" value.
        inv_Mat
}
