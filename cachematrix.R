


## The combination of makeCacheMatrix and cacheSolve takes a matrix 'x' and reports its inverse 's', first determining whether 's' is already cached. If it is, it report 's' without calculating it again.

## makeCacheMatrix creates a list of 4 functions that can be read by cacheSolve to determine whether the inverse of a matrix 'x' has already been calculated and cached ('s')

makeCacheMatrix <- function(x = matrix()) { 
	    s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated ('s'), then cacheSolve retrieves it from the cache. If not, it calculates and reports the inverse.

cacheSolve <- function(y, ...) { 
        s <- y$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- y$get()
        s <- solve(x) 
        y$setSolve(s)
        s
}