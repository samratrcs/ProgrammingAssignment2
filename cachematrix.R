## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  setMatrix <- function(y)
  {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  getMatrix <- function() x
  setMatrixInverse <- function(inverse) inverseMatrix <<- inverse
  getMatrixInverse <- function() inverseMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setMatrixInverse=setMatrixInverse,
       getMatrixInverse=getMatrixInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getMatrixInverse()
  if(!all(is.null(inverseMatrix))){
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$getMatrix()
  inverseMatrix <- solve(data)
  x$setMatrixInverse(inverseMatrix)
  inverseMatrix
}
