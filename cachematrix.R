## Put comments here that give an overall description of what your
## functions do

## A pair of functions that cache the inverse of a matrix

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {     # Creates a matrix that can cache its inverse
        i <- NULL                               # initialize the inverse operation
        set <- function(y) {                    # set the value of the matrix
                x <<- y
                i <<- NULL
        }
        get <- function() x                     # define the get function: get and return the value of the matrix
        setinv <- function(solve) i <<- solve   # set the value of the inverse of the matrix
        getinv <- function() i                  # get the value of the inverse of the matrix where called and return the value
        list(set = set, get = get,              # return a list of the operations. Needed to refer to the functions with the $ operator
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

# computes the inverse of the matrix returned by `makeCacheMatrix`
# if inverse exists (and matrix has not changed), it retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {                 # return a matrix that is the inverse of 'x'           
        i <- x$getinv()                 
        if(!is.null(i)) {                        # if the inverse exists already, return it
                message("getting cached data")
                return(i)
        }
        data <- x$get()                         # if inverse does not exist, get the matrix from the original matrix data
        i <- solve(data, ...)                   # solve for inverse operation
        x$setinv(i)                             # set the inverse to the matrix
        i                                       # return the inverted matrix
}


