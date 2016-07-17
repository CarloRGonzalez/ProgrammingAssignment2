makeCacheMatrix <- function(x = matrix(square)) {
  x_inverse <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) x_inverse <<- inverse
  getInv <- function() x_inverse
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}
cachesolve <- function(x, ...) {
  x_inverse <- x$getInv()
  if(!is.null(x_inverse)) {
    message("getting cached data")
    return(x_inverse)
  }
  matr <- x$get()
  x_inverse <- solve(matr, ...)
  x$setInv(x_inverse)
  x_inverse
}
##this functions are totally based on example functions "makevector" and "cachemean"
