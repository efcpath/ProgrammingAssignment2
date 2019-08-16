## The following functions simplify the process of computing
## an inverse matrix by calling upon cached values when able

## The following function creates a list of functions that work to:
## 1) Set the matrix
## 2) Get the matrix
## 3) Set the inverse of the matrix
## 4) Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  
## The following function will either:
## 1) compute the inverse of the matrix created by the function above or
## 2) retrieve the value of the previously calculated inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mtx <- x$get()
  i <- solve(mtx, ...)
  x$setinverse(i)
  i
}
