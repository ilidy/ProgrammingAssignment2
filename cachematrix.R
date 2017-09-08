## makeCacheMatrix returned the list of Matrix
## 1.ResetMatrix: redefine the new matrix if new matrix is assigned
## 2. DisplayMatrix: show the matrix you want to calculate
## 3. SetInverse: calculate the inverse matrix
## 4. GetInverse: show the inverse matrix if it was calculated


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


## Display the Inverse Matrix of provided one
## If Inverse Matrix was calculated prior, then get extra message "get Cached Inverse Matrix" additional

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



