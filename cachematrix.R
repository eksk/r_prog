
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.  Here, the below functions are to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		mm <- NULL
		set <- function(y) {
		    x <<- y
               	    mm <<- NULL
        }

	get <- function() x
        setinvs <- function(invs) mm <<- invs
        getinvs <- function() mm
        list(set = set, get = get,
             setinvs = setinvs,
             getinvs = getinvs)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	mm <- x$getinvs()
        if(!is.null(mm)) {
                message("getting cached data")
                return(mm)
        }
        data <- x$get()
        mm <- solve(data, ...)
        x$setinvs(mm)
        mm

}