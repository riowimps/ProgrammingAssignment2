## Functions that cache the inverse of a matrix

## Makes a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ##Setting the inverse to initialization
  i <- NULL
  
  ## Set the matrix
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Get the matrix
  get <- function() m
  
  ## Set the inverse of the matrix
  setinverse <- function(inverse) i <- inverse
  getinverse <- function() i
  
  ## Return a list of the methods
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## A function to compute the inverse of the matrix returned by "makeCacheMatrix"
## If the inverse has been gotten, then the cache inverse will be retrieved.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## Returns the inverse if it has been set
        if( !is.null(m) ) {
          message("getting cached data")
          return(m)
        }
        
        ## Gets the matrix
        data <- x$get()
        
        ## Computes the inverse
        m <- solve(data) %*% data
        
        ## Sets the inverse to the object
        x$setinverse(m)
        
        ## Returns the matrix
        m
}
