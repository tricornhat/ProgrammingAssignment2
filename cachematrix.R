## cachematrix.R
## A set of functions to create performant matrices that save computation
## time by caching their own inverses.
##
## N.B.: These functions assume the given matrix is invertible.
##
## Usage example:
##     mtrx <- matrix(...) # Some sort of matrix
##     cache_mtrx <- makeCacheMatrix(mtrx)     # Create a caching matrix
##
##     mtrx_inverse  <- cacheSolve(cache_mtrx) # Computes inverse matrix solution only once
##
##     mtrx_inverse <- cacheSolve(cache_mtrx)  # Subsequent invocations will return the
##                                             # solution computed previously from cache
##
##     mtrx_original <- cache_mtrx$get()       # Get the original un-inverted matrix



## This function creates a special "Cache Matrix" object that has the capability to
## cache its inverse. Use the cacheSolve(mtrx) function to obtain the inverse and
## to make use of the caching capability of this object.
##
## Parameter x is the matrix for which the inverse will be computed.
## 
## Returns a list of functions which can be passed to cacheSolve(mtrx).
##
makeCacheMatrix <- function(x = matrix()) {
        # invrs holds the inverse of the inuput matrix
        invrs <- NULL
        
        # set the base matrix and clear cache
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        
        # Return the base matrix
        get <- function() x
        
        # Put the inverse matrix into the cache
        setinverse <- function(inv) invrs <<- inv
        
        # Retrieve inverse from cache (may be NULL)
        getinverse <- function() invrs
        
        # Return named list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Get the inverse of the given Cache Matrix. If the inverse
## has already been computed, then it will be returned from cache.
## Otherwise the inverse is computed, cached, and returned.
##
cacheSolve <- function(x, ...) {
        # Return inverse from cache if possible
        invrs <- x$getinverse()
        if(!is.null(invrs)) {
                return(invrs)
        }
        
        # No inverse was found in cache, so create the inverse,
        # cache it, and return it.
        data <- x$get()
        invrs <- solve(data)
        x$setinverse(invrs)
        invrs
}
