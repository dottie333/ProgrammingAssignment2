## This program creates two functions: makeCacheMatrix and cacheSolve.  
## The makeCacheMatrix creates a "matrix" object that can cache its inverse.  
## CacheSolve computes the inverse of the matrix makeCacheMatrix. 

## This function will take a matrix object as an input to the makeCacheMatrix function.   
## The matrix input must be a square matrix.   
## The get() function is used to retrieve the format of the Matrix.  
## After running the function script, from the command line I executed the following statements:
##
#       > v <- makeCacheMatrix(matrix(c(1,2,3,4),nrow =2))
#       > v$get()


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
        
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)  
    
}
    
##  This function cacheSolve will create the inversion of the matrix
##  created by the makeCacheMatrix function. 


cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m

        ## To return the inverse of the function
        ## From the command line, I executed the fllowing statment.
        ##  > cacheSolve(v)
    
}
