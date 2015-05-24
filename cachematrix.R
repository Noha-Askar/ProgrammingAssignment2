## makeCacheMatrix creates a special "matrix" object that can cache its inverse, it 
## returns a list containing a function to set the value of the matrix, get the value of
## the matrix, set the value of the inverse of the matrix and get the value of 
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
            
            ## m is the initial value of the inverse of the matrix
            m <- NULL
            ## The set function sets the value of the matrix for the first time and 
            ## updates changes to matrix
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            ## get function gets the value of the matrix
            get <- function() x
            ## setinverse calculates the inverse of the matrix
            setinverse <- function(solve) m <<- solve
            ## getinverse gets the value of the inverse
            getinverse <- function() m
            list(set = set, get = get, 
                 setinverse = setinverse, 
                 getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the special "matrix" created with
## the makeCacheMatrix function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the matrix and sets the value
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
          m <- x$getinverse()
          ## checks to see if inverse is cached and if so, retreives it.
          if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
          }
          ## if the inverse is not cached, it is calculated then saved in the cache then 
          ## it is retreived.
          data <- x$get()
          m <- solve(data, ...)
          x$setinverse(m)
          m
}
