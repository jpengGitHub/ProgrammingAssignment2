## Programming assignment #2 of R programming course
##         Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation
## there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 

## The purpose of this assignment is to write a pair of functions that cache the inverse of a matrix
##      makeCacheMatrix, cacheSolve
## one should use them in pairs, 
##              i.e. call first function on a matrix
##              then call the second function on the output of the first function call

## Author: Jason Peng
## Date: 05/01/2016


## makeCacheMatrix function creates a special list containing a function to
##      set the value of the matrix
##      get the value of the matrix
##      set the value of the inverse of the matrix
##      get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    my_inv <- NULL
    set <- function(y) {
        x <<- y
        my_inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) my_inv <<- inverse
    getInverse <- function() my_inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## However, if first checks to see iff the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    my_inv <- x$getInverse()
    if(!is.null(my_inv)) {
        message("getting cached data")
        return(my_inv)
    }
    data <- x$get()
    ## if X is a square invertible matrix, then solve(X) returns its inverse
    my_inv <- solve(data)
    x$setInverse(my_inv)
    my_inv
}

#### To call this one first create a test matrix 
## orig_matrix <- matrix(c(4, 7, 2, 6), nrow=2, ncol=2)
## orig_matrix
##        [,1] [,2]
##[1,]    4    2
##[2,]    7    6
##

#### The call the first function and store the output
## result_matrix <- makeCacheMatrix(orig_matrix)

#### Then call the second function
#### Note: running it the first time -- no cache available
## cacheSolve(result_matrix)
##      [,1] [,2]
##[1,]  0.6 -0.2
##[2,] -0.7  0.4
#### Note: running it the second time -- this time the cache is used
## cacheSolve(result_matrix)
## getting cached data
##      [,1] [,2]
##[1,]  0.6 -0.2
##[2,] -0.7  0.4

#### End of this program.  Enjoy!
