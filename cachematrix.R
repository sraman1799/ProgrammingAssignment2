## Source - cachematrix.R
## Objective - To cache the inverse of a matrix
## Functions - (1) makeCacheMatrix: This function creates a matrix object that can cache its inverse.
##             (2) cacheSolve: This function computes the inverse of the matrix returned by above fn
## Constraints - The input should be an invertible square matrix. 



## makeCacheMatrix() - Constructor which returns a list of functions
## (1) makeMx(y)  - takes a matrix as argument and caches it
## (2) getMx()    - returns the cached matrix
## (3) setInvMx() - takes a inverse matrix as argument and caches it.
## (4) getInvMx() - retrieves the cached inverse matrix

makeCacheMatrix <- function(x= matrix()) {
    m <- NULL
    
    makeMx <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    getMx <- function() x
    setInvMx <- function(inv) m <<- inv
    getInvMx <- function() m
    
    list(makeMx = makeMx, getMx = getMx, setInvMx = setInvMx, getInvMx = getInvMx)
}



## cacheSolve() - function returns a matrix that is the inverse of input matrix:
## (1) First it checks whether the inverse of input matrix already exists in the cache and returns the 
##     cached inverse matrix if found.
## (2) Else, the function will get the data, calculate the inverse and call the setInvMx() to cache the 
##     calculated inverse for future use, if needed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInvMx()

    if(!is.null(m)) {
        message("getting cached Inverse MMatrix")
        return(m)
    }
    
    data <- x$getMx()
    m <- solve(data)
    x$setInvMx(m)
    m
}