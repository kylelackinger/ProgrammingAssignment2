## The two functions are designed to take as input a matrix and compute the inverse 
## of the supplied matrix.  If the inverse matrix has already been computed, the 
## result is pulled from cache instead of performing the calculation again.

## References: "Demystifying makeVector()" - https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md 

## This function takes as input a square matrix that is invertable and 
## creates an R object that stores the matrix and its inverse.  The returned object
## contains a copy of the defined makeCacheMatrix() environment.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function takes the makeCacheMatrix oject as its input
## and either computes the inverse of the input object's matrix or gets the 
## inverse matrix from cache if already computed

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
