# makeCacheMatrix is the function that creates the special Matrix
# It uses the get and set functionalities to set the Inverse of the Matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # set creates the special matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() { 
    x  # get returns the Matrix
  }
  
  # Just like set, the setInverse sets the Inverse Matrix
  setInverse <- function(Inverse) {
    m <<- Inverse
  }
  
  # getInverse function gets the Inverse Matrix
  getInverse <- function() {
    m
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# cacheSolve function is used to calculate the Inverse, 
# but if its already in cache, it returns the Inverse Matrix from cache.
# Function takes Matrix as the input
cacheSolve <- function(x = matrix()) {
  # Lets try to get the value makeCache
  m <- x$getInverse()
  
  # If the value of above expression is null then its not in cache.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  # solve function calculates the Inverse Matrix
  m <- solve(data)
  x$setInverse(m) # Set the calculated Inverse to cache
  m # Auto return the calculated Inverse Matrix
}

# Use the following expressions to test the matrix

z <- makeCacheMatrix(x=matrix(c(1,2,3,4),2,2))
y <- cacheSolve(z)
y
c <- cacheSolve(z)
# You should see a message called "getting cached data"
c # Print c
# -2, 1.5
# 1  -0.5
# Verify it in 
# http://www.algebra.com/algebra/homework/Matrices-and-determiminant/inverse-of-2x2-matrix.solver
