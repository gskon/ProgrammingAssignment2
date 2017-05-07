##  Matrix inversion is usually a costly computation and there may be 
##  some benefit to caching the inverse of a matrix 
##  rather than compute it repeatedly. So here are pair of functions
##  that cache the inverse of a matrix.


## For these functions to work properly, we assume that the matrix 
## supplied is always invertible.


## makeCacheMatrix: This function creates a special "matrix" object that    
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setInverse <- function(inverse) m <<- inverse
            getInverse <- function() m
            list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}

##cacheSolve: This function computes the inverse of the special "matrix" ##  returned by makeCacheMatrix above. If the inverse has already been          ##  calculated (and the matrix has not changed), then the cachesolve   ##  should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
          m <- x$getInverse()
          if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
