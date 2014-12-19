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

## Comments at the bottom show the code used to test these functions.

## Function ‘makeCacheMatrix’ returns a list of 4 functions:
## ‘set’ saves the input matrix in static var ‘x’ and
##  initializes static var ‘inverse’ to NULL
## ‘get’ returns the matrix stored in ‘x’
## ‘setinverse’ saves the inverse of ‘x’ in ‘inverse’
## ‘getinverse’ returns the value of ‘inverse’

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL #init the cached inverse
    set <- function(y) { #define fn to set cached matrix x
        x <<- y
        inverse <<- NULL #reset inverse for new matrix
    }
    get <- function() x #define fn to return cached matrix x
    setinverse <- function(i) inverse <<- i #define fn to cache inverse
    getinverse <- function() inverse #define fn to get cached inverse
    #now return list of these 4 fns
    list(set = set, 
         get = get,
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
    inverse <- x$getinverse() #retrieve from cache
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse) #return it without recomputing
    }
    data <- x$get() #get cached matrix
    inverse <- solve(data, ...) #compute its inverse
    x$setinverse(inverse) #save the inverse in cache
    inverse #and return that now-saved inverse
}

# The following tests were used on these functions:
# hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
# h8 <- hilbert(8)
# h8_inverse <- solve(h8)
# testCache8 <- makeCacheMatrix(h8)
# testCache8$get()
# testCache8$getinverse()
# cacheSolve(testCache8)
# testCache8$get() == h8
# testCache8$getinverse() == h8_inverse
# cacheSolve(testCache8)
# h9 <- hilbert(9)
# testCache9 <- makeCacheMatrix(h9)
# testCache9$get()
# testCache9$getinverse()
# cacheSolve(testCache9)
# cacheSolve(testCache9)
