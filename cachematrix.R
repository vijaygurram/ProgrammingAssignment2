## The 2 functions below create a special object that stores a matrix and caches its inverse.
## The code assume that the input matrix is invertible.

## 'makeCacheMatrix' : This first function returns a list that contains a set of functions 
## that are returned to the parent environment.
## The functions in the list store the 'x' matrix and create a cache that can store 
##the inversed matrix calculated from 'x' matrix.

makeCacheMatrix <- function(x = matrix()) {
    #initializing the cached value
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #fetch the value of matrix
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## 'cacheSolve' : This second function make use of the makeCacheMatrix function listed above.
## It returns the inverse of the matrix 'x'.
## It returns the value from the cache when it finds the match
## If this cache is empty, it calculates the inverse of x and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #create matrix since it doesn't exist
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
