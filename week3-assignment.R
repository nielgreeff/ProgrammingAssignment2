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
  
  my_inv <- x$getinv()
  # check if we have the inverse in cache already .. if not .. then calculate it
  if (!is.null(my_inv)) {
    
    message("getting from cache")
    return(my_inv)
  }
  
  # we got here ... no cache
  # setup the cache
  x$set(p)
    
  # calculate the inverse
  x$setinv()
  
  # display the inverse
  my_inv <- x$getinv()
  my_inv
  
}