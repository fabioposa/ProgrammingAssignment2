## the function makeCacheMatrix for each new matrix put m in NULL values the you can call 
## get etinverse then keep m variable set from cacheSolve.
## funtion get give the origial input matrix
## setmatrix is call from cacheSolve to put in m (external variable) the inverse matrix.
## getinverse give the result "inverse matrix" is was call in cacheSolve 
## Example of execution 
## input_example<-matrix(2:5, nrow=2, ncol=2)
## input_example
##     [,1] [,2]
## [1,]    2    4
## [2,]    3    5
## You can call 
## example<-makeCacheMatrix(input_example)
## example$get()         # Returns original matrix
## example$get()
##     [,1] [,2]
## [1,]    2    4
## [2,]    3    5
##
## First time get inverse not have setting (variale m) the result and return NULL
## example$getinverse()
## NULL
## You can set using cacheSolve
## cacheSolve(example)
##    [,1] [,2]
##[1,] -2.5    2
##[2,]  1.5   -1
## Now we have in m the inverse matrix in variable m
## if we call example$getinverse() now is setting 
##     [,1] [,2]
##[1,] -2.5    2
##[2,]  1.5   -1
## But if we can call another time example$getinverse() the variable m is setting and tell that "getting cached data"
## and don't calculate another time the inverse matrix, we just have in variable m
## cacheSolve(example)
##getting cached data
##     [,1] [,2]
##[1,] -2.5    2
##[2,]  1.5   -1
## if we call makeCacheMatrix  with another matrix we set m to null and we start as the first example.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(matrix) m <<- matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
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
        x$setinverse(m)
        m
}
