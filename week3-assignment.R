makeCacheMatrix <- function (x = matrix()) {
  inv <- NULL
  
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function() inv <<- solve(x)
  getinv <- function() inv
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function (x,p) {
  # setup the cache
  x$set(p)
  
  # calculate the inverse
  x$setinv()
  
  # display the inverse
  x$getinv()
}