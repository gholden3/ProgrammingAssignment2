# Matrix inversion is usually a costly computation and there may be some benefit to caching the
# inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix
# inversion that we will not discuss here). Your assignment is to write a pair of functions that 
# cache the inverse of a matrix. 

# Computing the inverse of a square matrix can be done with the solve function in R. For example,
# if X is a square invertible matrix, then solve(X) returns its inverse.

# For this assignment, assume that the matrix supplied is always invertible.


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setMatrix <- function(matrixToSet) {
      x <<- matrixToSet
      inverse <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inverseToSet) inverse <<- inverseToSet
    getInverse <- function() inverse
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
# should retrieve the inverse from the cache.
cacheSolve <- function(mx, ...) {
    inverse <- mx$getInverse()
    if(!is.null(inverse)) {
      message("got cached data. returning it.")
      return(inverse)
    }
    matrix <- mx$getMatrix()
    message("did not get cached data. calculating...")
    inverse <- solve(matrix, ...)
    mx$setInverse(inverse)
    art <- c("                __", "               /\\/'-,", "       ,--'''''   /\"", 
           " ____,'.  )       \\___", "'\"\"\"\"\"------'\"\"\"`-----'")
    cat(art, sep = "\n")
    invisible(inverse)
}

