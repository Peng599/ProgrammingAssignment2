## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## retrun special matrix:
##   set ==> assign x with input matrix y
##   get ==> return x
##   setSolve ==> assign m with solved reverse matrix
##   getSolve ==> returned cached reverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(Solve) m <<- Solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x':
## first check if the reverse of the matrix esists,
##   if reverse already cached, return it
##   if not, solve the reverse, retrun it after cache the reverse
cacheSolve <- function(x, ...) {  
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached reverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setSolve(m)
  m
}
