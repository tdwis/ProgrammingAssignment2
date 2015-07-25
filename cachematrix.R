## Put comments here that give an overall description of what your
## functions do
##
## cachematrix.R contain 2 functions to provide caching mechanism for inverse 
## operation on matrixs:
##   makeCacheMatrix - creates the cache matrix
##   cacheSolve - check if the cache exist already or not. if not, perform inverse operation and put the reuslt
##                to the cache.  If yes, retrieve result from the cache without computing it

## Write a short comment describing this function
##
## The function "makeCacheMatrix" construct a cache/special matrix, which is a list containing function to:     
##	set the matrix
##	get the matrix
##	set the inverse of the matrix
##	get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse, 
             getInverse = getInverse) 
}


## Write a short comment describing this function
##
## The function cacheSolve first check if the matrix is already in the cache or not,
## if not, perform inversion operation and put the result to the cache
## if yes, get the result from the cache without computing it
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	m <- x$getInverse ()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
