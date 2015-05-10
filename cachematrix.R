## The program contains two functions that cache the inverse of a matrix
## Written by DayanL 

## The first function makeCacheMatrix creates a special matrix, containing
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) 
  {
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


## The second function returns the inverse of matrix. The functions checks if 
## the inverse has been computed. If so, the function keep the result and skips
## the computation. Otherwise, it computes the inverse and sets the value in cache
## by solve function

cacheSolve <- function(x, ...) 
{
  m <- x$getinverse()
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
