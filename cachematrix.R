## Put comments here that give an overall description of what your
## functions do

## The functions are used in the following way
## makeCacheMatrix recieves a matrix as an argument and returns a special
## cached matrix. This cached matrix can be used by cacheSolve to 
## either calculate the inverse of the matrix directly, or retrieve it 
## from memory if it has been calculated previously by this function


## Write a short comment describing this function
## Stores the matrix in a cachable object
## This object is a list that contains a function to:
## 1 Set the value of the matrix
## 2 Get the value of the matrix
## 3 Set the value of the inverse
## 4 Get the value of the inverse
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


## Write a short comment describing this function
## This function calculates the inverse of a matrix stored in
## the special object created by makeCachedMatrix.
## If the inverse of the matrix has already been calculated
## and it is stored, the function skips the calculation
## and retrieves the inversed matrix from memory
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
