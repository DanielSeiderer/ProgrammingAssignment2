## The below two functions 'makeCacheMatrix' and 'cacheSolve' are
## used to create a special object that stores a matrix and caches
## its inverse


## The first function 'makeCacheMatrix' creates a list containing
## functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  
  ## Defining the function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  
  ## Defining the function to get the value of the matrix
  get <- function() x
  
  ## Defining the function to set the value of the inverse
  setinverse <- function(inverse) inv_x <<- inverse
  
  ## Defining the function to set the value of the inverse
  getinverse <- function() inv_x
  
  ## Creating the list containing the above four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function 'cacheSolve' checks whether the value of the
## matrix M  has changed. If it's still the same matrix, it then
## checks whether the cached inverse exists. If the cached inverse
## exists, it then returns that value. If the matrix M has changed or
## if the cached inverse doesn't exist, it calculates the inverse of M
## and returns the value.
##
## Pleae note that 'cachSolve' requires an additional input variable
## 'M' as compared to the instructions. This is to enable it to check
## whether the value of M has changed and to ensure that it returns
## the correct value for the inverse of M.

cacheSolve <- function(M,x, ...) {
  ## Check whether the value of M has changed
  if(identical(M,x$get())){
    inv_x <- x$getinverse()
    
    ## Check wether the cached inverse of M exists. If this is the
    ## case, the message 'getting cached data' will be printed and the
    ## cached value will be returned.
    if(!is.null(inv_x)) {	
      message("getting cached data")
      return(inv_x)
    }
  }
  
  ## In case that the value of M has changed, or in case that the
  ## cached inverse doesn't exist, the inverse of M will be calculated
  ## and the cache will be updated.
  x$set(M)
  data <- x$get()
  inv_x <- solve(data, ...)
  x$setinverse(inv_x)
  inv_x
}
