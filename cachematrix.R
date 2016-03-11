## This function captures a Matrix into the vector makeCacheMatrix, and then we use cacheSolve function to give the inverse of the matrix

## makeCacheMatrix, is used to create a function which has 4 other functiones in it, basically to set up a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This funcion cachesolve, gives the inverse of a matrix set by cachematrix, if the inverse matrix was given before it will just give the matrix stored in cache, otherwise it will calculate a new inverse of matrix

cachesolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
