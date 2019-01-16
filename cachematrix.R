## Process to cache the inverse of a matrix to avoid a costly computation.
## Instead of compute the inverse of a matrix from the same input repeatedly, save data to cache and retreive data from cache can reduce computation time and resources.

## function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    # initial value to inverse of matrix
    inv_mat <- NULL
    
    # function to set data of matrix from specify input
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    
    # function to get data of matrix
    get <- function() x
    
    # function to set data of inverse matrix to environment by specify input
    setInv <- function(mat) inv_mat <<- mat
    
    # function to get data of inverse matrix from environment
    getInv <- function() inv_mat
    
    # list of available functions
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## Retreive cache matrix
    inv_mat <- x$getInv()
    
    ## check if cache of invese matrix is exist, if cache is available return cache data 
    if(!is.null(inv_mat)) {
        message("getting cached matrix")
        return(inv_mat)
    }
    
    # in case cache of inverse matrix is not exist, compute inverse matrix, set cache and return data
    mat <- x$get()
    inv_mat <- solve(mat, ...)
    x$setInv(inv_mat)
    inv_mat
}
