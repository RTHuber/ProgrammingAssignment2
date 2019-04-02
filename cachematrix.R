makeCacheMatrix <- function(X = matrix()) {
  I <- NULL
  set <- function(y) {
    X <<- y
    I <<- NULL
  }
  get <- function() X
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(X, ...) {
  I <- X$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- X$get()
  I <- solve(data, ...)
  X$setinverse(I)
  I
}
