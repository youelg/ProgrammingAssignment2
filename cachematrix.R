##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y){
    x<<-y
    invmatrix <- NULL
  }
  get <- function () x
  setinverse <- function(inverse) invmatrix <<- inverse
  getinverse <- function() invmatrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getinverse()
  ## Returning cached inverse of a matrix if it has already been computed
  if(!is.null(invmatrix)){
    message("Inverse matrix has already been computed and cached")
    return(invmatrix)
  }
  # Computing the inverse of a matrix if it has not been done yet
  data <- x$get()
  invmatrix <- solve(data, ...)
  x$setinverse(invmatrix)
  return(invmatrix)
}
