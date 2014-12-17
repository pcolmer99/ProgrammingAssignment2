## New file (c) Paul Colmer 2014
## Two functions that calculate the inverse of a matrix and ensure it is cached.
## If the same inverse calculation is encountered the cached matrix is retrieved.
## This reduces the time required to calculate computations.

## The makeCacheMatrix function creates a special "matrix" object that can cache it's inverse:
## Set the value of the matrix (set)
## Get the value of the matrix (get)
## Set the value of the inverse of the matrix (setinverse)
## Get the value of the inverse of the matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) {
        m <<- solve
    }
    getinverse <- function() {
        m
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function calculates the inverse matrix created 
## with the makeCacheMatrix function.
## It checks to see if the calculation exists.  If so it returns the cached calculation.
## Otherwise it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting Cached Data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    ## 'm' returns a matrix that is the inverse of 'x'
}
