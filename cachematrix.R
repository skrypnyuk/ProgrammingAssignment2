## Functions to avoid unnecessary calculations of inverse matrices by only calculating them the first time.
# Call e.g. mycachematrix<-makeCacheMatrix(mymatrix) and then cacheSolve(mycachematrix)   

## This functions creates a structure with two fields (matrix and its inverse) and with four functions (which are returned by the function)
## for accessing and setting them. If no matrix is given as an input, then it is initialised by the empty matrix. It is possible
## to directly get and set the inverse, but the idea is to rather do it through the function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inver) inv <<- inver
                       
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function takes as an argument an object created by makeCacheMatrix and either returns
## the already calculated inverse matrix (which was saved previously) or returns a newly calculated
## inverse and saves it in its input for eventual future use

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  
  inv
}
