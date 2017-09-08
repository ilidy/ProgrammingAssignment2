## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  reset <- function(a){
    x <<-  a
    m <<- NULL
  }
  get <- function() x
  setinv <- function(x) m <<- solve(x)
  getinv <- function() m
  list(ResetMatrix = reset, DisplayMatrix = get,
       SetInverse = setinv, GetInverse = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$GetInverse()
  if(!is.null(inv)){
    message("get Cached Inverse Matrix")
    inv 
  }
  data <- x$DisplayMatrix()
  inv <- solve(data, ...)
  x$SetInverse(inv)
  inv
}
