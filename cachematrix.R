## The functions below are used to create a special object that stores a matrix and cache's its invers. 
## The functions assume that the matrix passed is inversible.

## makeCacheMatrix creates a special "matrix", which is in fact a list (and returns this list) that containes functions to 
##      1. set the value of the matrix and set the invers to null
##      2. get the value of the matrix
##      3. set the value of the invers of the matrix
##      4. get the value of the invers of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invers <- NULL
        set <- function(y) {
                x <<- y
                invers <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invers <<- solve
        getinverse <- function() invers
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## The function below computes the invers of the special "matrix". 
## It takes as parameter the special object created by the function makeCacheMatrix
## First it checks if the invers has already been computed, if so, it displays a message that the cached value
## is used and returns the cached invers. If the invers is not in the cache, it gets the matrix, computes its invers 
## by using the "solve" function and sets the value of the invers using the "setinvers" function. After that it
## returns the invers.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invers <- x$getinverse()
        if (!is.null(invers)) {
                message("getting cached inverse")
                return(invers)
        }
        data1 <- x$get()
        invers <- solve(data1, ...)
        x$setinverse(invers)
        invers
}
