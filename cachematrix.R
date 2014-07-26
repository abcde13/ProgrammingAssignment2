## This is my super ultra inverse-of-matrix caching script. All it does
## is cache the inverse of a matrix for quicker access times.

## makeCacheMatrix creates a cacheMatrix for an original matrix.
## It handles setting and getting the matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL               # i is the variable storing the inverse
    set <- function(y){     # set() will set the matrix to a value
        x <<- y
        i <<- NULL          # on first set, the inverse will be null,
                            # because we haven't calculated it yet
    }
    get <- function() {     # get() merely returns the current matrix
        x
    }
    setInverse <- function(inverse){    # setInverse() will set the inverse
        i <<- inverse                   # of the matrix. 
    }
    getInverse <- function(){           # getInverse() merely returns the 
        i                               # current matrix's inverse
    }
    
    #Return from function provides methods to use on current matrix
    list(set = set, get=get, setInverse = setInverse, getInverse=getInverse)
}


## cacheSolve takes an argument of type create by makeCacheMatrix
## and either solves for its inverse and stores it, or finds that 
## it has already been calculated. At the end, it returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getInverse()     # First, get inverse of cached matrix x (into m)
    if(!is.null(m)){        # If it isn't null, then we have already calculated
        message("getting inverse from cache") # it, so return it
        return(m)
    }
    data <- x$get()                 # Otherwise, get the matrix stored in x 
    inverse <- solve(data,...)      # Solve for its inverse
    x$setInverse(inverse)           # Store inverse in x for future use
    inverse                         # return the inverse for viewing
}
