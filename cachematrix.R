## These functions work in tandem to compute and cache the inverse of a given 
## square, invertible, matrix

## makeCacheMatrix caches an input matrix as well as its inverse within
## its computing environment and returns a list of functions

makeCacheMatrix <- function(x = matrix()) {     # x must be a square invertible 
                                                # matrix
        inv <- NULL     # Creates a place to cache the inverse matrix
        set <- function(y) {    # y is the input matrix x
                x <<- y         # x is cached in the makeCacheMatrix environment
                inv <<- NULL    # Resets the cached inverse in makeCacheMatrix 
        }                       # environment to disassociate with a new matrix
        get <- function() x     # Returns the input matrix that is cached
        setinverse <- function(inverse) {       # Caches the input inverse,  
                inv <<- inverse                 # which is externally computed,
        }                                       # into the makeCacheMatrix
                                                # environment
        getinverse <- function() inv    # Returns the inverse matrix that is
                                        # cached in the makeCacheMatrix
                                        # environment
        list(set = set, get = get,      # Creates a list of the functions, where
             setinverse = setinverse, getinverse = getinverse) # each is named
}                                                              # after itself


## cacheSolve takes a list created by makeCacheMatrix and either returns the 
## cached inverse, if it has been cached, or computes it, caches it in the 
## makeCacheMatrix environment, then returns it

cacheSolve <- function(x, ...) {        # x must be a list in the format 
                                        # returned by makeCacheMatrix
        inv <- x$getinverse()   # Stores the cached inverted matrix for use
        if(!is.null(inv)) {     # Verifies cached inverse already exists
                message("getting cached data")  # Signals cache already exists
                return(inv)     # Returns the existing cached inverse matrix
        }
        data <- x$get()         # If there was no cached inverse, stores 
                                # original matrix, that must exist, for 
                                # computing the inverse
        inv <- solve(data, ...)         # Computes original matrix's inverse
        x$setinverse(inv)       # Caches the computed inverse into the list
        inv     # Returns the computed inverse
}
