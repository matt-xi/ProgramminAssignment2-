## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

## both functions below function to cache the inverse of a matrix
## this function creates a special "matrix" object able to cache its inverse
makeCacheMatrix <- function(x = matrix()) { ## define argument w/ default "matrix" mode
  inv <- NULL                               ## initialize inv as NULL; holds value of matrix inv
  set <- function(y) {                      ## define set function to assign new
    x <<- y                                 ## value of matrix in the parent environment
    inv <<- NULL                            ## if there is new matrix, set inv to NULL
  }
  get <- function()x                        ## define get function - return value of matrix arg
  
  setInverse <- function(solveMatrix) inv <<- solveMatrix ##assign value of inv to parent environmet
  getInverse <- function() inv                            ##gets value of inv where called
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ##this is to help 
                                                                               ##refer to functions 
                                                                               ## with $ operator
}

## Write a short comment describing this function

## The function will compute inverse of the special "matrix" returned by the 
## makeCacheMatrix
## If inverse is already calculated and the matrix did not change, 
## cacheSolve will then get the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
