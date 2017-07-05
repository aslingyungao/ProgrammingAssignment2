##Lingyun Gao 
##Peer-graded Assignment: Programming Assignment 2: Lexical Scoping

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse) inv <<- inverse
  getsolve <- function() inv
  list(set = set, 
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
  
