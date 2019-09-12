## These functions create a matrix cache object with functions for
## managing a cached inverse matrix, calculating the inverse, and
## retrieving the cached inverse if available.

## makeCacheMatrix: Accepts a matrix as an input argument and creates
## an object in the Global Environment with functions for setting and
## getting the passed matrix and its inverse (computed and stored
## by a separate function. The passed matrix and its inverse are
## stored (cached) in the function evaluation environment.

makeCacheMatrix <- function(A = matrix()) {
        m_inv_cache <- NULL
        setmat <- function(arg1) {
                A <<- arg1
                m_inv_cache <<- NULL
        }
        getmat <- function() A
        setinv <- function(m_inv) m_inv_cache <<- m_inv
        getinv <- function() m_inv_cache
        list(setmat = setmat, getmat = getmat,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve: Accepts a matrix cache object, created with
## makeCacheMatrix as its input argument and checks if the 
## current inverse value is NULL. If its NULL, it computes 
## the inverse of the cached matrix and stores it in the 
## object's environment. If the current inverse value is not 
## NULL it prints a message and returns the cached inverse.

cacheSolve <- function(CacheObj, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- CacheObj$getinv()
        if(!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        data <- CacheObj$getmat()
        m_inv <- solve(data, ...)
        CacheObj$setinv(m_inv)
        m_inv
}
