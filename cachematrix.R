## R Programming Assignment 2: Lexical Scoping
## Assignment : Caching the Inverse of a Matrix

## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse
## makeCacheMatrix returns the list of functions for the matrix

makeCacheMatrix <- function(x = matrix()){ 
        
        ##Initially inverse should be NULL
        inv <- NULL 
        
        ## set function set's the matrix assignment/ creation of matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        ## We have only assigned the matrix here and inverse function 
        ## is not called yet, hence inv variable should be NULL
        }
        
        ## get function returns the set matrix
        get <- function() x
        
        ## Set's the inverse matrix 
        setInverse <- function(inverse) inv <<- inverse
        
        ## This function returns the inverse of the matrix
        getInverse <- function() inv
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function . 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.
## It is assumed that the matrix supplied will always be invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        ## It is checked that whether inverse matrix exists
        ## Initially when inverse is not calculated this is skipped
        ## Because inverse has not been calculated
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## mat variable has the original matrix
        mat <- x$get()
        
        ## if mat is a square invertible matrix, then solve(mat) returns its inverse
        inv <- solve(mat, ...)
        
        ## inverse of the matrix is passed to setInverse
        x$setInverse(inv)
        
        ## Inversed matrix is returned
        inv
}
