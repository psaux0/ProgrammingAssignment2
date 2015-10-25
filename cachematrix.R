##Calculate a the inverse matrix of matrix by cache

## This function set up a special matrix that you can set or get its value
## while you can also get its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  set <- function(y) {
    x <<- y
    r <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) r <<- inv
  getinverse <- function() r
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)

}


## If the inverse matrix was cached, then use the cache as the result
## If it is not, then calculate it and cache it then

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  r <- x$getinverse()
  if(!is.null(r)) {
    message("getting cached data")
    return(r)
  }
  
  d <- x$get()
  r <- solve(d)
  x$setinverse(r)
  r
}
