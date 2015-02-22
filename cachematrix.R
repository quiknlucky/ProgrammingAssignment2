## makeCacheMatrix creates a matrix object with sub functions
## cacheSolve takes a matrix object and returns the inverse

## take a matrix as a parameter to create a matrix
## object which contains several sub functions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## set function so matrix can be changed after initialization
  set <- function(y) {
    x <<- y
    ## set inv to null when ever matrix is changed 
    inv <<- NULL
  }
  ## return the current matrix
  get <- function() x
  ## calculate the inverse of the current matrix
  calc_inverse <- function() inv <<- solve(x)
  ## return the inverse of the current matrix
  get_inverse <- function() inv
  ## assign values to refer to sub functions
  list(set = set, get = get,
       calc_inverse = calc_inverse,
       get_inverse = get_inverse)
}

## return the inverse of a matrix by checking if cached value 
## of the inverse exists and return that otherwise 
## calculate the inverse
cacheSolve <- function(x, ...) {
  ## get the cached inverse 
  retval <- x$get_inverse()
  ## set print statement
  prn <- ("getting cached matrix")
  ## if the inverse does not exist, calculate it
  if(is.null(retval)){
    ## calcualte the inverse
    x$calc_inverse()
    ## put inverse in return value
    retval <- x$get_inverse()
    ## if calculating inverse update print statement
    prn <- ("calculating inverse matrix")
  }
  ## print statement
  print(prn)
  ## return the inverse of x
  return(retval)
}
