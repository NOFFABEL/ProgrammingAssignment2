## this function build and inverse of the given matrix. The inverse is cached to optimize processing.

## Cache the given matrix if it is invertible
makeCacheMatrix <- function(x = matrix()) {

  if(nrow(x) != ncol(x)) {
    message("this matrix is not invertible.")
    return()
  }
  
  # the inverse of the given matrix.
  x_ <- NULL
  
  set <- function(y) {
    x <<- y
    x_ <<- NULL
  }
  get <- function() x
  
  setinv <- function(inv) x_ <<- inv
  getinv <- function() x_
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## process the inverse and cached the result.
cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  x_ <- x$getinv()
  if(!is.null(x_)){
    message("getting cached inverse")
    return(x_)
  }
  mtx <- x$get()
  x_ <- solve(mtx, ...)
  x$setinv(x_)
  x_

}
