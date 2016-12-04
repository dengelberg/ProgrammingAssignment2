## This following functinos create a cached matrix object and cache the
## inverse of the matrix in the cached matrix obejct.

## This first function creates and manages the cached matrix object.

makeCacheMatrix <- function(x = matrix()) {
        iMat <- NULL
        set <- function(y){
                x <<- y
                iMat <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) iMat <<- inverse
        getInverse <- function() iMat
        list(set = set, get = get,
             masetInverse = setInverse,
             getInverse = getInverse)
}


## This function caches the inverse of the matrix in the cached matrix object

cacheSolve <- function(x, ...) {
        iMat <- x$getInverse()
        if(!is.null(iMat)){
                message("Getting cached data")
                return(iMat)
        }
        data <- x$get()
        if(nrow(x$get()) == ncol(x$get())) {
                iMat <-  solve(data, ...)
                x$setInverse(iMat)
        }
        else message("Function has no inverse")
        iMat
        ## Return a matrix that is the inverse of 'x'
}