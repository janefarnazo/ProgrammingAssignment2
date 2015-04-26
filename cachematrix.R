
# R Programming Assignment 2

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {                     ## It receives a matrix variable
        inverse <- NULL                                           
        set <- function(y) {                                    ## Sets variables and functions in memory
                cache_x <<- y                                   
                cache_m <<- NULL                                          
        }
        get <- function() cache_x                               ## Create a function to get the matrix passed in the command line call
        set_cache_m <- function(inverse) cache_m <<- inverse    ## Create a function to pass the value of cache_m in cache to the value of local_m           
        get_cache_m <- function() cache_m                       ## Create a function to pass value of cache_m from cache and return it to validate if it's NULL
        list(set = set, get = get,                              ## Returns a list of functions nested within makeCacheMatrix
             set_cache_m = set_cache_m,
             get_cache_m = get_cache_m)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x) {                                     ## Receives makeCacheMatrix from the caller
        inverse<- x$get_cache_m()                               ## Get the value from cache environment
        if(!is.null(inverse)) {                                 ## Validate if Null   
                message("getting cached data")                  ## If null, return the value with a message 
                return(inverse)
        }                                       
        startingmatrix <- x$get()                               ## Call the nested function x$get in makeCacheMatrix to get the uninverted matrix        
        returnmatrix <- solve(startingmatrix)                   ## Use solve() to invert the startingmatrix
        x$set_cache_m(returnmatrix)                             ## Call nested function x$set_cache_m() in makeCacheMatrix to set m in the cache environment to the local non-NULL inverted result in returnmatrix
        returnmatrix                                            ## Evaluate returnmatrix so as to return it to caller if cache_m is non NULL
}
