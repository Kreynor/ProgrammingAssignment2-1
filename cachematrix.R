## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
  mat_inv <- NULL
  set <- function(y){
    mat <<- y
    mat_inv <<- NULL
  }
  get <- function() mat
  set_inv <- function(inv_mat) mat_inv <<- inv_mat
  get_inv <- function() mat_inv
  list(set = set, get = get, get_inv = get_inv, set_inv = set_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m_cache <- x$get_inv()
  if(!is.null(m_cache)){
    message("getting cached inverted matrix")
    return(m_cache)
  }
  data <- x$get()
  mat_out <- solve(data, ...)
  x$set_inv(mat_out)
  mat_out
}
