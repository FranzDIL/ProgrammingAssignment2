
## makeCacheMatrix takes a matrix as input and also stores certain properties
## with it - like the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv,
         getinv = getinv)
}



## cacheSolve computes and assigns the inverse of a makeCacheMatrix object.
## Before computing the inverse cacheSolve checks whether an inverse for 
## the makeCacheMatrix object has already been computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


m <- matrix(runif(25),5,5)
s <- makeCacheMatrix(m)
cacheSolve(s)
