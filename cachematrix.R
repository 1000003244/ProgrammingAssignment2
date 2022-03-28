
## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## I'm defining the argument with default mode of "matrix".
    Inv <- NULL                             ## I initialize 'Inv' as NULL; will hold value of matrix inverse. 
    set <- function(y) {                    ## I define the set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        Inv <<- NULL                        ## if there is a new matrix, reset 'Inv' to NULL.
    }
    get <- function() x                     ## I define the get fucntion - it returns the value of the matrix argument.

    setinverse <- function(inverse) Inv <<- inverse  ## It assigns value of 'Inv' in parent environment.
    getinverse <- function() Inv                     ## It gets the value of 'Inv' where called.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## I use this to refer 
                                                                                  ## to the functions with the $ operator.
}

## This function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse is already been calculated and the matrix has not changed values,
## then cacheSolve will recover the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## This function returnes a matrix that is the inverse of 'x'
    Inv <- x$getinverse()
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data, ...)
    x$setinverse(Inv)
    Inv
}

