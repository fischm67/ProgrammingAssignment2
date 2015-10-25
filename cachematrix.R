## Programming Assignment 2
## The makeCacheMatrix and cacheSolve functions are included below

## makeCacheMatrix will create an object in the global environment which allows cacheSolve to work on creating the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                                           ## instantiate a local m variable to allow cacheSolve to assign
    if(!is.matrix(x)) {                                 ## check to make sure initial value is a matrix
        stop("Must be a matrix")                        ## if not send message
    }

    set <- function(y) {                                ## define function set, overrides original matrix and resets 
                                                        ## global m variable
        if(!is.matrix(y)) {                             ## check to make sure you are assigning a matrix data type
            stop("Must be a matrix")
            }
        else {
            x <<- y                                     ## if a matrix then use superassignment to write over original
            m <<- NULL                                  ## superassign cached global variable so cacheSolve will
        }                                               ## re-compute the inverse
    }
    get <- function() x                                 ## returns the current matrix
    setInverse <- function(setImatrix) {                ## untilized by cacheSolve to store inverse matrix in 
                                                        ## global variable m
        if(!is.matrix(setImatrix)) {                    ## check to make sure you are assigning a matrix data type
            stop("Must be a matrix")
            }
        else {
            m <<-setImatrix                             ## use superassignment to store inverse matrix
        }
    }
    getInverse <- function() m                          ## returns inverse matrix
    list(set = set, get = get,                          ## list out available functions with the makeCacheMatrix object
         setInverse = setInverse,
         getInverse = getInverse)
}

## Assuming current matrix has an inverse cacheSolve will compute

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()                                 ## utilize getInverse function from makeCacheMatrix object
    if(!is.null(m)) {                                   ## if global m variable has been reset to NULL cacheSolve will 
                                                        ## compute inverse
        message("getting cached data")                  ## output message for cached inverse matrix being returned
        return(m)
    }
    data <- x$get()                                     ## if global m variable is NULL utilize get function from 
                                                        ## makeCacheMatrix to get current matrix
    m <- solve(data, ...)                               ## assign local m variable to the inverse matrix
    x$setInverse(m)                                     ## utilize setInverse setting global m variable to latest inverse matrix
    m                                                   ## return inverse matrix to user
}
