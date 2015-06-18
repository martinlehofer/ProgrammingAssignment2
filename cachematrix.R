## Put comments here that give an overall description of what your
## functions do


## Creates a function(environment) that can store the matrix and also the
### getters and setters for the inverse of thematrix
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


## Returns a inversed matrix. Function must be created with the function
### makeCacheMatrix. This function stores the inversed matrix in the 
### $m object of the environment created by the constructor function 
### makeCacheMatrix(). If function has been called  before for an 
### environment this function returns the stored value instead of
### solving the matrix again.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                print("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
