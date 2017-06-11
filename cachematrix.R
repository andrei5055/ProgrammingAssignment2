## makeCacheMatrix - creates an object of the matrix type, which allows  
##                   to use the cached results of its inversion
## NOTE: the input parameter x could be any (n x m) matrix

makeCacheMatrix <- function(x = matrix()) {
    inversedMatrix <- NULL
    
    # Create list of methods for manipulation with the matrix
    list(set = function(y) { x <<- y; inversedMatrix <<- NULL}, 
         get = function() x,
         setInversedMatrix = function(invMatr) inversedMatrix <<- invMatr,
         getInversedMatrix = function() inversedMatrix)
}


## cacheSolve - inverts the matrix, created by makeCacheMatrix
## NOTE: this function will try to invert only the square matrices of any size

cacheSolve <- function(x, ...) {
    invMatr <- x$getInversedMatrix()
    if(!is.null(invMatr)) {
        return(invMatr)  # Getting cached data
    }

    # Check if matrix is a square matrix 
    y <- x$get()
    len <- length(y) 
    size <- sqrt(len)
    if (len != size * size) {
        message("Cannot invert matrix - it is not a square matrix")
        return(NaN)
    }
    
    # Calculate, store and return the inverse matrix
    invMatr <- solve(y, diag(size), ...)
    x$setInversedMatrix(invMatr)
    invMatr
}
