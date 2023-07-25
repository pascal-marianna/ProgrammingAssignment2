## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## the first function "makeCacheMatrix" creates a special matrix containing a function that sets the value of
## the matrix, gets the value of the matrix, sets the inverse of the matrix and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) minv <<- inverse
  getinverse  <- function() minv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## the next function calculates the inverse of the special "matrix" created with the above function.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getinverse()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinverse(minv)
  minv
}
