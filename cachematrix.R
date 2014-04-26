## This script is for caching the Inverse of a Matrix.
## Function makeCacheMatrix creates a special type of matrix
## that remembers its Inverse matrix.
## Function cacheSolve takes that type of matrix as input and
## output the inverse of the input matrix


## Function that creates a special type of matrix, 
## which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Function that takes the special "matrix" 
## returned by makeCacheMatrix as input and
## output the inverse of that input matrix
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    
    # If cached data exists
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Else compute the inverse and save to cache
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}
