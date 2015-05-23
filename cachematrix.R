## the function makeCacheMatrix for each new matrix put m in NULL values the you can call 
## get etinverse then keep m variable set from cacheSolve.
## funtion get give the origial input matrix
## setmatrix is call from cacheSolve to put in m (external variable) the inverse matrix.
## getinverse give the result "inverse matrix" is was call in cacheSolve 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getinverse <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getinverse = getinverse)

}


## Write a short comment describing this function
##if function cacheSolve have m (external variable not null ) make the inversion matrix of the matrix input 
##and give the result in m


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
