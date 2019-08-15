## Cache matrix inversion

## Create a matrix object to cache it's inverse
## Example of usage cacheSolve(makeCacheMatrix(matrix(c(1, 3, 2, 4), 2,2)))

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function () {
    x
  }
  
  setInverse <- function (inverseMatrix) {
    
    if (hasArg(inverseMatrix))
      # Set inversed matrix
      m <<- inverseMatrix
    else
      # Invert current matrix
      m <<- solve(x)
  }
  
  getInverse <- function () {
    m
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns a matrix inverted from x
## x should come from makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  if (is.null(m)) {
    m <- solve(x$get())
    x$setInverse(m)
  }
  
  m
}
