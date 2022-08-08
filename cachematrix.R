## Below are two functions that are used to create a special object that stores 
## a matrix and cache's its inverse. We use the <<- operator to assign a value to
## an object in an environment that is different from the current environment.

## The first function creates a special matrix: The first function creates a 
## special array: set the value of the vector, get the value of the vector, 
## set the value of the inverse, get the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This second function calculates the inverse of the special matrix created 
## with the "makeCacheMatrix" function. It first checks if the inverse has 
## already been computed. If so, it gets the inverse of the cache and skips the 
## calculation. Otherwise, it calculates the inverse of the matrix and sets the 
## inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
