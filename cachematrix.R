## Put comments here that give an overall description of what your
## functions do
# The two functions makeCacheMatrix and cacheSolve make a cache of a matrix and
# it's inverse (if provided) so that the inverse is not calculated repeatedly


## Write a short comment describing this function
# This function creates a special "matrix", that is nothing but a list
# containing the functions to
# 1.    set the matrix
# 2.    get the matrix
# 3.    set the inverse of the matrix
# 4.    get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    # The inverse of the matrix x
    inv_x <- NULL
    
    # Set the values of the matrices
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    
    # Get the original matrix
    get <- function() x
    
    setinverse <- function(mat) {
        inv_x <<- mat
    }
    
    getinverse <- function() inv_x
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# The following function calculates the inverse of the special "matrix" created
# with the above function. It first checks to see if the inverse has already
# been calculated. If it is, it gets the inverse matrix from the cache and skips
# the computation. Else, it calculates the inverse of the matrix and assigns it
# in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Create an identity matrix with the same dimensions as that of x
    identity_matrix <- diag(nrow = nrow(x$get()), ncol = ncol(x$get()))
    
    inv_x <- x$getinverse()
    if(!is.null(inv_x)) {
        if(inv_x %*% x$get() == identity_matrix) {
            message("Getting cached data.")
            return(inv_x)
        } else {
            message("The matrix passed as an argument is not the same used to
                    store the inverse. Exiting.")
            return(NULL)
        }
    }
    data <- x$get()
    inv_x <- solve(data)
    return(inv_x)
}
