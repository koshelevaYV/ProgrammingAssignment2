# Caching the Inverse of a Matrix

# Creates matrix for inputting to cacheSolve() function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # Set the value 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Get the value 
  get <- function() x
  
  # Inversing the matrix 
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# Given a makeCacheMatrix object, returns the inverse 
# If the inverse has already been calculated 
# then the cacheSolve() returns the cached inverse
cacheSolve <- function(x, ...) {
  
  m <- x$getsolve() 
  # Cached matrix?
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  
  # cache is empty
  # create inverted matrix
  data <- x$get()       
  m <- solve(data, ...) 
  x$setsolve(m)	      
  m		      
}
