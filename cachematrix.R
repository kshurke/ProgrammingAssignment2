## makeCacheMatrix() creates our special matrix which we will use to 
## cache its own inverse. 

## Example: 
## matrix <- matrix (rnorm(16), nrow = 4)
## specialMatrix <- makeCacheMatrix(matrix)
## specialMatrix$get()
## cacheSolve (specialMatrix)
## cacheSolve (specialMatrix)
## cacheSolve(specialMatrix)
## ...retrieving from cache... /when called second time, it retrives 
## from cache

makeCacheMatrix <- function(x = matrix()) {
  
  #create empty inverse cache
  inverse <-NULL
  
  #set the matrix
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  #get the matrix
  get <- function () x
  
  #set the inverse of the matrix
  setInverse <- function(inv) inverse <<-inv
  
  #get inverse of the matrix
  getInverse <- function() inverse
  
  #return the matrix and the new functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes the inverse of the matrix. If the inverse is already calculated, it will return its cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getInverse ()
  
  #if the inverse already calcualted, return it
  if (!is.null(inverse)){
    
    message ("...retrieving from cache...")
    return (inverse)
  }
  
  #if not calcualted, calculate the inverse
  data <-x$get()
  inverse <- solve (data, ...)
  
  #cache the inverse
  x$setInverse (inverse)
  
  #return the inverse
  inverse
}
