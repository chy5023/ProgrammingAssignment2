## Author: Philip A. Indlekofer
## 17 Feb. 2015
## Code acknowleged where author known, e.g., R. Peng et al from Johns Hopkins
## adoption of the "makeVector()" and "cachemean()" functions
## solve(m) %*% m to invert matrix where m is matrix
## To test: 
## 1. assign makeCacheMatrix() to an object, e.g., mkMatrixCache <- makeCacheMatrix()
## 2. create a matrix, e.g, 1:16,4,4, by executing mkMatrixCache$set(matrix(1:16,4,4))
## 3. execute cacheSolve(myMatrixCache)

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(cacheMatrix = matrix()) {
        invMatrix <- NULL
        set <-function(y){
                cacheMatrix <<- y
                invMatrix <<- NULL
        }
        get<-function() cacheMatrix
        setMatrix <-function(solve) invMatrix<<- solve
        getMatrix <-function() invMatrix
        list(set=set, get=get, setMatrix=setMatrix, getMatrix=getMatrix)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cache solve() should retrieve the 
## inverse from the cache.

cacheSolve <- function(cacheMatrix=matrix(), ...) {
        ## Return a matrix that is the inverse of 'cacheMatrix'
	  invMatrix <- cacheMatrix$getMatrix()
	  ## if inverse already available, return it from cache
        if(!is.null(invMatrix)){
             message("getting cached data")
             return(invMatrix)
        }
        matrix <- cacheMatrix$get()
        invMatrix <- solve(matrix, ...)
        cacheMatrix$setMatrix(invMatrix)
        invMatrix
}
