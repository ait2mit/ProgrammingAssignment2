#This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
# x must be square and invertible matrix
# caching matrix
  invrs <- NULL
  set <- function(y) {
    x <<- y 
    invrs <<- NULL
  }
  
  # Getting cached matrix
  get <- function() x
  
  # caching inverse matrix
  setinvrs <- function(inverse) invrs <<- inverse
  
  # Getting cached invert matrix
  getinvrs <- function() invrs
  # return a list
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve retrieve the inverse of the matrix.

cacheSolve <- function(x, ...) {
  invrs=x$get()
  # If inverse already cached, it does not calculate just retrieve the inverse matrix
  if(!is.null(invrs)){
    message("getting cached data")
    return(invrs)
  }
# Calculate the inverse of the matrix
  matrx=x$get()
  invrs=solve(matrx)
  x$setinvrs(invrs)
    
  return(invrs)
}

