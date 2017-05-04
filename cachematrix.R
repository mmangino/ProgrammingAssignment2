## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#Return the inverse if it's cached
#example: mat<-matrix(c(7, -2, 3, 5), ncol=2, nrow=2)
# fancy <- makeCacheMatrix(mat)
# cacheSolve(fancy)

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
