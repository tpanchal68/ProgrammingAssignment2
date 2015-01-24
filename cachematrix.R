## Tejash Panchal
## R Programming assignment 2

## makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse.
## cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix function.  It also calculates the Determinant of
## given matrix.  If Determinant is 0, the function will print a message
## since it cannot calculate inverse.  If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieve the inverse
## from the cache.

## Program can be executed using steps below:

# > a<-makeCacheMatrix(matrix(c(1:6, 6,4,7),3,3))
# > a$get()
# [,1] [,2] [,3]
# [1,]    1    4    6
# [2,]    2    5    4
# [3,]    3    6    7
# > a$getinverse()
# NULL
# > cacheSolve(a)
# [,1]       [,2]       [,3]
# [1,] -0.7333333 -0.5333333  0.9333333
# [2,]  0.1333333  0.7333333 -0.5333333
# [3,]  0.2000000 -0.4000000  0.2000000
# > a$getinverse()
# [,1]       [,2]       [,3]
# [1,] -0.7333333 -0.5333333  0.9333333
# [2,]  0.1333333  0.7333333 -0.5333333
# [3,]  0.2000000 -0.4000000  0.2000000
# > cacheSolve(a)
# getting cached data
# [,1]       [,2]       [,3]
# [1,] -0.7333333 -0.5333333  0.9333333
# [2,]  0.1333333  0.7333333 -0.5333333
# [3,]  0.2000000 -0.4000000  0.2000000
# > 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## inverse a matrix, if the matrix is inversed already, return the cached-inversed one, 
## otherwise, inverse it and save the inversed matrix in cache and return it

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        ## Calculate the Determinant of a Matrix to make sure it's non-zero
        determinant <- det(data)
        if (determinant != 0) {
                m <- solve(data, ...)
                x$setinverse(m)
                m                
        } else {
                message("Calculated Determinant of provided Matrix is 0; thus, cannot calculate inverse.")
        }
}


