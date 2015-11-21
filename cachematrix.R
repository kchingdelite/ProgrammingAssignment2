## I know that this matrix is invertible, and I know its inverse: 
## a <- matrix(c(2,3,2,1,2,1,1,1,2),ncol=3)
## x <- makeCacheMatrix(a)
## cacheSolve(x)
## we get 
##      [,1] [,2] [,3]
##[1,]    3   -1   -1
##[2,]   -4    2    1
##[3,]   -1    0    1

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## create m = NULL locally
  m <- NULL
  ## set x = y passed on via set function
  ## set m = NULL globally
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## this function will simply retreive stored value of x
  get <- function() x
  ## this function will set value of m to 'inverse' passed on
  ## via this function globally
  setinverse <- function(inverse) m <<- inverse
  ## will simply display m
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   ## first check to see if inverse already calculated for x
   ## if yes then simply return stored value
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if not stored then use the x data passed on
  data <- x$get()
  ## Return a matrix that is the inverse of 'x'
  m <- solve(data, ...)
  ## reteive stored inverse and then display it
  x$setinverse(m)
  m
}
