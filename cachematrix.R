##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
##This function creates a vector which is list containing following functions
##1.set the value of the vector
##2.get the value of the vector
##3.set the value of the inverse of matrix
##get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x <<- y 
        m <<- NULL 
    }
    get <- function() x 
    setinverse <- function(inverse) m <<- inverse 
    getinverse <- function() m 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned
##by makeCacheMatrix above.
##The inverse is  already been calculated by above function and matrix has not changed
##The cachesolve function returns a matrix that is the inverse of 'x' from cache.

    cacheSolve <- function(x, ...) {
               
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
    }

