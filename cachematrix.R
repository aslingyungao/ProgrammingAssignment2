##Lingyun Gao 
##Peer-graded Assignment: Programming Assignment 2: Lexical Scoping

## The function below is used to calcualte the inverse of a matrix.
## Because it takes long time to compute the inverse of a mega-size matrix, it make sense to cache the value of
## the inverse of matrix to avoid unnecessary repeated calculations.
## The main idea is to make it possible to access to the value of the invere of matrix directly by storing it in a seprated function,
## instead of having to call the makeCacheMatrix() and compute its inverse again in the cacheSolve()function
## This method works for time-consuming computations


## The Method is executed by two steps

## makeCashedMatrix() is used to pass a matrix x and create an "object" of type "list". 
## It stores the original matrix value and what will be cached, the inverse of matirx.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # inv will ve our 'inverse of matrix'
  set <- function(y) { # takes an input matrix
    x <<- y            # saves the input matrix
    inv <<- NULL       #resets the inverse to NulL
  }
  get <- function() x  # this function returns the value of the original vector
  
  
  setsolve <- function(inverse) inv <<- inverse  # this is called by cacheSolve() during the first cacheSolve()
    #access and it will store the value using superassignment
  
  getsolve <- function() inv  # this will return the cashed value to casheSolve( )on subsequent accesses
  
  list(set = set, 
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## casheSolve()accesses the obejct, instead of the makeCaheMatrix().
##If the inverse matrix has not yet been calcualted, cacheSolve()calcualates the inverse matrix and stores it
## in the object created by the call to makeCaheMatrix(),then returns the inverse matrix.
## if the inverse has been calculated,simply fetches it and return the inverse matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getsolve() # accesses the object 'x' and get the value of the inverse
    
  if (!is.null(inv)) {    # if inverse was already cached(Not Null)
    message("getting cached data")   # send this message to the console
    return(inv)                      # and return the inverse..."return" ends
  }
  data <- x$get()     # we reach this code only if x$getsolve()returned Null
  inv <- solve(data, ...)   # if inv was Null then we have to calculate the inverse
  x$setsolve(inv)     # store the calcuated mean value in x (see setmean() in makeVec)
  
    inv   # Return a matrix that is the inverse of 'x'
}
  












