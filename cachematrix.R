## These two functions "makeCacheMatrix" and "cacheSolve" can be used to 
## create a special matrix capable of caching its inverse so that the inverse
## matrix does not need to be recomputed.

## makeCacheMatrix takes a matrix and returns a list of four functions:
## 1. set - set the value of the matrix
## 2. get - get the value of the matrix
## 3. set.inv - set the value of the matrix inverse
## 4. get.inv - get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    matrix.inv <- NULL
    
    set <- function(y) {
        x <<- y
        matrix.inv <- NULL
    }
    
    get <- function() x
    
    set.inv <- function(inv) matrix.inv <<- inv
    
    get.inv <- function() matrix.inv
    
    list(set = set, get = get,
            set.inv = set.inv,
            get.inv = get.inv
         )
}


## cacheSolve takes a special matrix created with the makeCacheMatrix function
## and returns the inverse of the matrix. The function first checks if the 
## inverse is stored in cache, and if so, returns it. If the inverse is not
## stored in cache, it is calculated, stored in cache, and returned.

cacheSolve <- function(x, ...) {
    mat.inv <- x$get.inv()
    
    if(!is.null(mat.inv)) {
        message("Getting cached inverse...")
        return(mat.inv)
    }
    
    mat <- x$get()

    mat.inv <- solve(mat, ...)
    
    x$set.inv(mat.inv)
    
    mat.inv
}
