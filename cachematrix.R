## Put comments here that give an overall description of what your
# Functions
#   makeCacheMatrix - Create a matrix object that can cache its inverse.
#   cacheSolve - Compute the inverse of the matrix returned by makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        # Reset x and clear the cached inverse
        set <- function(y) {
                # value of x,s to parent environment.
                x <<- y
                s <<- NULL
        }  
        # 3 functions are returned: get, setsolve, getsolve
        get <- function() x
        setsolve <- function(solve) s <<- solve  
        getsolve <- function() s
        # Return the list of functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# If the inverse has already been calculated (and the matrix is unchanged!)
#  then cacheSolve retrieves the inverse from the cache.
 cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        # if no data cached, use solve() on the matrix to get inverse
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}