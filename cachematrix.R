## This function creates a special "matrix" object.
## The inverse of the matrix can be cached then.

makeCacheMatrix <- function(x = matrix()) {
    
    ## To set the inverse of the matrix to NULL.
    inv_matrix <- NULL
    
    ## To set the matrix x to a new matrix y.
    ## To reset the inverse of the matrix to NULL.
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    
    ## To return the matrix x.
    get <- function() x
    
    ## To set the inv_matrix to inverse of the matrix.
    setInv <- function(inverse) inv_matrix <<- inverse
    
    ## To return the inverse the the matrix.
    getInv <- function() inv_matrix
    
    ## To return the defined special matrix containing all the functions.
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function computes the inverse of the matrix created.

cacheSolve <- function(x, ...) {
    ## To get the inverse of the matrix and store it in inv_matrix.
    inv_matrix <- x$getInv()
    
    ## If the inverse has already been computed with the same matrix,
    ## then the inverse is reused.
    if (!is.null(inv_matrix)) {
        message("Getting cached data...")
        return(inv_matrix)
    }
    
    ## To store the matrix x in m.
    m <- x$get()
    
    ## To solve the matrix x for its inverse.
    inv_matrix <- solve(m, ...)
    
    ## To call the function setInv defined previously.
    x$setInv(inv_matrix)
    
    ## To return a matrix that is the inverse of 'x'
    inv_matrix
        
}
