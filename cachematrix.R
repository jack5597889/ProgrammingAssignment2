## The makeCacheMatrix function creates a special 'matrix' object 
## that can cache its inverse and the cacheSolve function computes the 
## inverse of the special 'matrix' retutned by makeCacheMatrix. 
## If the inverse has been calcualted then retrieve the inverse 
## from the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## set a vairable to store the inverse ##
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

cacheSolve <- function(x, ...) {
  m <- x$getInv() ##get the inversed matrix from x
  if(!is.null(m)){
    message("getting cache data")
    return(m)
  }
  data <- x$get() # if not, get the matrix object
  m <- solve(data) # solve the inverse
  x$setInv(m)
  m        
}


