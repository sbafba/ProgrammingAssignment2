## The cachematrix.R file contains two functions, makeCacheMatrix() and cacheSolve(). 
## The first function in the file, makeCacheMatrix() creates an R object that stores a matrix and its inverse.
## The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix()
## in order to retrieve the inverse from the cached value that is stored in the makeCacheMatrix() object's environment.


## The first function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {    ## Initialize objects: x and i
        i <- NULL
        ## Define the "behaviors" or functions for objects of type makeCacheMatrix(): 
        set <- function( matrix ) {                           ## Method to set the matrix
                x <<- matrix                                  
                i <<- NULL
        }
        get <- function() x                                   ## Method to get the matrix
        setInverse <- function(inverse)  i <<- inverse        ## Method to set the inverse of the matrix
        getInverse <- function() i                            ## Method to get the inverse of the matrix
        
        list(set = set, get = get,                            ## Create a new object by returning a list()
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()                          
        if( !is.null(i) ) {                          ## Return the inverse if its already set   
                message("getting cached data")
                return(i)
        }
        data <- x$get()                              ## Get the matrix from our object
        i <- solve(data, ...)                        ## Calculate the inverse 
        x$setInverse(i)                              ## Set the inverse to the object
        i                                            ## Return the matrix
}
