## The use of these two functions in conjunction allow a user to calculate once
## and recall the inverse of a static square matrix. This will allow efficiency
## in higher level coding by eliminating the need to recalculate on an as needed
## basis.
##------------------------------------------------------------------------------

## This function acts as an initializing function setting a 4 item list variable
## example use:  a <- makeCacheMatrix(matix(rnorm(n*n),n,n)))
##                       where n is a positive integer value

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL 
     set <- function(y) {
            x <<- y
            i <<- NULL
     }
     get <- function() x
     setinverse <- function(inv) i <<- inv
     getinverse <- function() i
     list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Using this function will check for an exsisting inverse and return it.
## Otherwise it will utalize the "solve" function and assign the resulting value
## to the array created with the above function.

cacheSolve <- function(x) {
     ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data)
     x$setinverse(i)
     i
}
