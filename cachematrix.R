## This returns a special type of matrix that can have its inverse cached.
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function()
        x
    setinverse <- function(solve)
        cache <<- solve
    getinverse <- function()
        cache
    list(
        set = set, get = get, setinverse = setinverse,getinverse = getinverse
    )
}


## cacheSolve will return an inverse of the CacheMatrix or calculate it and return it if needed.

cacheSolve <- function(x, ...) {
    cache <- x$getinverse()
    if (!is.null(cache))
    {
        message("Cache Hit")
        return(cache)
    }
    data <- x$get()
    message("Cache Miss... Calculating...")
    cache <- solve(data,...)
    x$setinverse(cache)
    cache
}
