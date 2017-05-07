##  Matrix inversion is usually a costly computation and there may 
##  be some benefit to caching the inverse of a matrix 
##  rather than compute it repeatedly. So here are pair of
##  functions that cache the inverse of a matrix.


## For these functions to work properly, we assume that the matrix 
## supplied is always invertible.


## makeCacheMatrix: This function creates a special "matrix" object    
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
			# Define function to set the value of the matrix. It also 			# clears the old
			# inverse from the cache.         
         
            set <- function(y) {
                    x <<- y 		# Set the value
                    m <<- NULL 	# Clear the cache
            }
            
            # Define function to get the value of matrix
            
            get <- function() x
            
            # This is to define the inverse. It will be used by 				# getinverse() when there is no cache inverse.
            
            setInverse <- function(inverse) m <<- inverse
            
            # Define function to get the inverse.
            
            getInverse <- function() m
            
            # Returns the list with above four functions.
            
            list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}

##cacheSolve: This function computes the inverse of the special 
##  "matrix" returned by makeCacheMatrix above. If the inverse has         
## already been calculated (and the matrix has not changed), then    
##  the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
          m <- x$getInverse() #Fetches cached value for inverse
          if(!is.null(m)) {   # If cache not empty, just return it
                  message("getting cached data")
                  return(m)
  }
  
  # Cache was empty. It will calculate it,cache it and return it.
  
  data <- x$get()			# Get value of matix.
  m <- solve(data, ...)		# Calculate the Inverse.
  x$setInverse(m)			# Cache the result.
  m							# Returns the Inverse.
}
