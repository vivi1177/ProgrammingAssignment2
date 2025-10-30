## makeCacheMatrix: create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                      # cached inverse (NULL means "not computed yet")
  
  set <- function(y) {             # replace the matrix; drop cached inverse
    x <<- y
    inv <<- NULL
  }
  get <- function() x              # return the current matrix
  
  setinv <- function(i) inv <<- i  # cache the inverse
  getinv <- function() inv         # fetch cached inverse (or NULL)
  
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}



## Write a short comment describing this function
## makeCacheMatrix: create a special matrix object that stores a matrix and its cached inverse.
## It returns a list of four functions: set(), get(), setinv(), getinv().
## cacheSolve: return the inverse of the matrix stored in a makeCacheMatrix object.
## If the inverse is already cached, it returns the cached value; otherwise it computes,
## caches, and returns the inverse (using solve()).

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {             # if already cached, return it
    message("getting cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)           # compute inverse (assumes 'mat' is invertible)
  x$setinv(inv)                    # cache for next time
  inv
}

