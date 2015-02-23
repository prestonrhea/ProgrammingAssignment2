## These functions manipulate the object classes in R
## to make it easy to store the inverse value of a matrix
## in order to recall it in the future, without recomputing it.

## makeCacheMatrix will take in a matrix x, and use it to make a list
## with four entries: set the value of a matrix with set,
## get the value of the matrix with get, 
## set the matrix's inverse value with with setinverse,
## get the matrix's inverse value with getinverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes in a list (which in this case we make with makeCacheMatrix)
## checks to see if the inverse is already created and stored,
## if the inverse of the matrix is not stored, it computes it with solve()
## and stores the inverse value in the list.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}