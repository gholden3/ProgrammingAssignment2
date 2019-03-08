cached <- function() {
  print('Testing inverse of a matrix which should cache its result')
  mx <- matrix(c(1,0,-2,3,1,-2,-5,-1,9), nrow = 3, ncol = 3)
  cmx <- makeCacheMatrix(mx)
  cacheSolve(cmx)
  cacheSolve(cmx)
}

uncached <- function() {
  print('Testing inverse of a matrix which should not have a cached result')
  mx <- matrix(c(1,0,-2,3,1,-2,-5,-1,9), nrow = 3, ncol = 3)
  cmx <- makeCacheMatrix(mx)
  cacheSolve(cmx)
  cmx$setInverse(NULL)
  cacheSolve(cmx)
}