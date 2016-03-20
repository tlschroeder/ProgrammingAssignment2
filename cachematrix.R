## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  
  
  #saves the matrix, clears any saved inverse
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  #returns the matrix  
  get <-function() x
  
  #saves the inverse
  setinverse <-function(inverse)m <<- inverse
  
  #gets the inverse
  getinverse <- function()m
  
  #returns internals to make them accessible externally
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
  
}


##Computes the inverse of the special matrix, retrieves it if it has
##already been calculated.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #checks for pre-existing solution and returns if found
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #calculates and saves new inverse if necessary
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
