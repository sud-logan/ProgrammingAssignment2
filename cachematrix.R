## These functions calculate the inverse of a matrix and stores it as cache
## so that next time the inverse is needed, they don't have to be calculated.

## This function returns a list which creates the function for saving and
## retrieving the values from the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)

}


## This function takes the list created by above function as argument and
## calls the various functions to cache the inverse and retrieve it upon need

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  else{
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
  }
        ## Return a matrix that is the inverse of 'x'
}
