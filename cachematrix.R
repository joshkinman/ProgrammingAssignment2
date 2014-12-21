## Calculates inverse of matrix and stores value
## If inverse is needed again, the value will be retrieved from cache and not calulated again


makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMat <- function(inverse) m <<-inverse
  getInvMat <- function() m
  list(set = set, get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
  
}
## cacheSolve calculates the inverse of matrix returned by makeCacheMatrix
## if inverse has already been calculated, it will be retrieved from the cache


cacheSolve <- function(x, ...) {
  m <- x$getInvMat()           ##retrieves value from makeCacheMatrix
  if ( ! is.null(m)) {          ##checks to see if m contains data
    print("getting cached data") 
    return(m)                   ##If inverse is already calculated it is retrieved from cache
  }
  m <- solve(x$get())           ##Calculates inverse if it is not already in cache
  x$setInvMat(m)
  m
}
