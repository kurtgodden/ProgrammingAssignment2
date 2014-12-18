## These functions are used to compute the inverse 
## of a square, invertible matrix. The first time
## it’s computed, it is stored in a cache defined
## by the static variable ‘inverse’ within the closure
## defined by the function ‘makeCacheMatrix’.
## Function ‘cacheSolve’ takes an output of makeCacheMatrix
## and either computes and saves
## the inverse of the input matrix (if it has not yet
## been computed/saved) or else it retrieves
## a previously computed/saved inverse.

## Function ‘makeCacheMatrix’ returns a list of 4 functions:
## ‘set’ saves the input matrix in static var ‘x’ and
##  initializes static var ‘inverse’ to NULL
## ‘get’ returns the matrix stored in ‘x’
## ‘setinverse’ saves the inverse of ‘x’ in ‘inverse’
## ‘getinverse’ returns the value of ‘inverse’

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function ‘cacheSolve’ takes the output from
## makeCacheMatrix and computes the inverse of matrix ‘x’,
## unless it has already been computed.  If it has, then it 
## simply retrieves the previously computed inverse.
## Else, it computes and saves the inverse, invoking the helper
## functions created by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
