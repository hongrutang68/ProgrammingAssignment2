
## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly 

## This function is to store a martix and a cached value of the inverse of the 
## matrix.

makeCacheMatrix = function(x = matrix()) {
  cache = NULL
  #Set matrix
  setMatrix <- function(y) {
    x <<- y
    cache <<- NULL
  }
  #Get matrix
  getMatrix = function() {
    x
  }
  #Set inverse
  setInverse = function(inverse){
    cache <<- inverse
  } 
  #get inverse
  getInverse = function(){
    cache
  }
  list(setMatrix = setMatrix,getMatrix = getMatrix,
       setInverse = setInverse,getInverse = getInverse )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve = function(x, ...) {
  cache = x$getInverse()
  if (!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  matrix_data = x$getMatrix()
  cache = solve(matrix_data, ...)
  x$setInverse(cache)
  cache
}

## Test my functions
matrix = matrix(rnorm(9),3,3)
my_matrix = makeCacheMatrix(matrix)
my_matrix$getMatrix()
my_matrix$getInverse()

cacheSolve(my_matrix) #first time getting the inverse
cacheSolve(my_matrix) #second time getting the inverse




