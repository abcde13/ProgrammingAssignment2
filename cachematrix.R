?m## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting inverse from cache")
        return(m)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setInverse(inverse)
    inverse
}
