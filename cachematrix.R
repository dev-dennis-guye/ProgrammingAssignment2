## This file contains operations that create and operate on caches
## makeCacheMatrix takes an input matrix and creates a list that caches
## results of operations on the matrix
## cacheSolve function accepts a cache and calculates the inverse of a matrix

## Creates a list of functions that acts can act as a cache of operations on
## a matrix
makeCacheMatrix <- function(x = matrix()) {
    # Initially set the result to null
    res <- NULL
    set <- function(y) {
        x <<- y ## set the value of the matrix who's result can be found in the cache
        res <<- NULL
    }
    get <- function() x ## get matrix
    ## stores the result
    setCachedResult <- function(mean) res <<- mean
    ## returns the result
    getCachedResult <- function() res
    list(set = set, get = get,
         setCachedResult = setCachedResult,
         getCachedResult = getCachedResult)
}

## Creates the inverse of a matrix
## x in the cache of a matrix and ... is an additional arguments to be passed to
## solve()
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cachedResult<-x$getCachedResult()
    if(!is.null(cachedResult)){ ## check if the cached result exists
        ## and if it does return
        return(cachedResult)
    }
    else{
        ## otherwise compute the cached result
        data<-x$get()
        inverseMatrix<-solve(data,...)
        ## store in the cache for future use
        x$setCachedResult(inverseMatrix)
        return(inverseMatrix)
    }
}

cache<-makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,4,0),nrow = 3, ncol = 3))
res<-cacheSolve(cache)

## should be
#   1    2    3
#1 -24   18   5
#2  20  -15  -4
#3  -5   4    1
print(res)
