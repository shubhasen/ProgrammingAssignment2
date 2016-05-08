## Since matrix inversion is a costly computation
## it helps to cache the inverse of a matrix
## rather than compute it every time

## given a matrix as input
## create getter/setter functions
## to store in cache and retrieve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setCache <- function(mtx) m <<- mtx
        getCache <- function() m
        list(set = set, get = get, 
             setCache = setCache, getCache = getCache)
        
}


## given a special matrix object
## first check if the inverse of the matrix
##   is already in cache
## if it is return from cache,
##    else invert it, store in cache
#3    and also return  the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <-  x$getCache()
        if (!is.null(m)) {
                message("getting inverse of catched matrix...")
                return(m)
        }
        
        dat <- x$get()
        m <- solve(dat)
        x$setCache(m)
        m
}
