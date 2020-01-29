## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # The inverse of the matrix x
    inv_x <- NULL

    # set <- function(y) {
    #     x <<- y
    #     inv_x <<- NULL
    # }

    # Get the original matrix
    get <- function() x

    setinverse <- function(mat) {
        inv_x <<- mat
    }
    getinverse <- function() inv_x

    list(get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinverse()
    if(!is.null(inv_x)) {
        message("Getting cached data.")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data)
    return(inv_x)
}
