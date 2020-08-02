## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      #Input Matrix eg: matrix(1:4, nrow = 2, ncol = 2)
      #Set inverse default value as NULL
        inv <- NULL
      #Set matrix value
      set <- function(y){
      #cache the matrix - assigns value y from parent environment
            x <<- y
      #check the parent environments for existing variable and set to NULL
            inv <<- NULL
      }
      #Get the matrix value cached with setmatrix
      get <- function() {
	  x
	  }
      # Cached value of inverse matrix is saved in inv
      setInverse <- function(inverse) {
	  inv <<- inverse
	  }
      #Get the saved value of inverse matrix from inv
      getInverse <- function() {
	  inv
	  }
      #Create List of functions
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      #Returns inverse of matrix x eg: x <- matrix(1:4, nrow = 2, ncol = 2)
      inv <- x$getInverse()
      #Check id cacheSolve has been run before
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      #Gets the matrix
      mat <- x$get()
      #Calculates the inverse of matrix
      inv <- solve(mat, ...)
      #Sets the inverse value
      x$setInverse(inv)
      #Returns the inverse
      inv
        
}
