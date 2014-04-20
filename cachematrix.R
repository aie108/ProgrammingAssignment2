## makeCacheMatrix: used to create a matrix with cachable inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## initial the inverse matrix of x with NULL
  inverse <- NULL
  
  ## set function: assign mat to x and NULL to inverse
  set <- function(mat) {
    x       <<- mat
    inverse <<- NULL
  }
  
  ## get function: return x
  get <- function() x
  
  ## setinverse/getinverse: used to set and get 
  ## cached inverse matrix, accordingly.
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## getinverse from matrix x
  inverse <- x$getinverse()
  
  ## if inverse was ever computed, return the existing one.
  if(!is.null(inverse)) {
    message("getting cache data")
    returm(inverse)
  }
  
  ## in case no cached inversed matrix, do the following:
  ## get matrix x
  data <- x$get()
  
  ## calculate the inverse and then set the cache.
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  
  ## return inverse
  inverse
}
