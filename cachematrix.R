## makeCacheMatrix constructs a matrix with functions to get/set 
## the matrix inverse

## usage: matrix <- makeCacheMatrix(m), where m is your matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<-NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get=get,
             setinv = setinv,
             getinv = getinv)
}

## function to return matrix inverse. Stored in cache if already called.
## Usage: matrixInv <- cacheSolve(matrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m  
}
