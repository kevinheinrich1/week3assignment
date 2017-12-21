## These functions computes the inverse of a matrix and stores it in
## a cache so it doesn't have to be computed again. The purpose of
## this is to save time. Computing inverses of matrices can be time-
## consuming. 

## makeCacheMatrix sets the value of the matrix, gets the value,
## sets the value of the inverse and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m<<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix created
## in makeCacheMatrix, but only after checking to see if the 
## inverse has already been calculated. If the inverse has
## already been calculated, it gets the inverse from the cache
## instead of calculating it again. This is where the time-saving
## happens.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
