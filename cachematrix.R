## These functions are used to cache a matrix and it's inverse. This pair of funcitons can be
## used to simplify and speed up the calculation of matrix inverses.

## makeCacheMatrix creates an object that returns a list of 4 functions. In addition, the function
## caches the original matrix, and the inverse of the matrix if given in an external environmennt
## for returning those values through the get() and get_inv() functions.

makeCacheMatrix <- function(mat = matrix() #input is required and must be matrix) {
  mat_inv <- NULL #initialize the inverted matrix in the local function envinronment
  set <- function(y){ #create a function to set the passed matrix to the external environment for caching
    mat <<- y #cache the passed matrix
    mat_inv <<- NULL #cache a placeholder for the inverted matrix
  }
  get <- function() mat #function to return the passed matrix from the cache environmentn
  set_inv <- function(inv_mat) mat_inv <<- inv_mat #caches the inverted matrix
  get_inv <- function() mat_inv #returns cached inverted matrix
  list(set = set, get = get, get_inv = get_inv, set_inv = set_inv) #output object as a list of above functions
}


## This function takes an object created by 'makeCacheMatrix' as input and calculates the inverse for the cached matrix
## stored in the object. If the inverse matrix has already been calculated, 'cacheSolve' simply returns the cached
## inverted matrix.

cacheSolve <- function(x, ...) {
  # check to see if the inverted matrix has already been calculated 
  m_cache <- x$get_inv() 
  if(!is.null(m_cache)){
    message("getting cached inverted matrix")
    return(m_cache) #If already calculated, return cached inverted matrix
  }
  #Code below this point only to be executed if the cached inverse matrix value is NULL
  data <- x$get() #Return cached matrix
  mat_out <- solve(data, ...) #Calculate inverse for matrix
  x$set_inv(mat_out) #Cache the inverse matrix
  mat_out #Return inverse matrix
}
