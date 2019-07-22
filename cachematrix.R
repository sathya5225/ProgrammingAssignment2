## the function creates an object that stores the matrix and caches its inverse. makeCacheMatrix creates a matrix 
## which sets and gets the matrix as well as inverse

## the function is creating x, which stores the matrix under given logic statements.

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y){
  x <<- y
  i <<- NULL
}
get <- function(x)
  setinverse <- function(inverse) 
    i <<- inverse
  getinverse <- function(i)
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This function computes inverse of the matrix created by makeCacheMatrix. The cacheSolve draws the inverse of the Cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  }
        
A <- matrix(c(100, 99, 98, 97),2,2)
A1 <- makeCacheMatrix(A)
cacheSolve(A1)
