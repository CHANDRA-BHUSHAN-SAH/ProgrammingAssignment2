## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

## Matrix variable invrs to store inverted matrix
  invrs <- NULL

## set function to set the original matrix i.e. x with some value or another matrix
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }

## get function to retrieve the value of the original matrix i.e. x
  get <- function() x

## setInvers function to cache the inverse of matrix x
  setInvers <- function(inv) invrs <<- inv

##  getInvers function to retrieve  the cached value of inverse matrix i.e. invrt
  getInvers <- function() invrs

  list(set = set, get = get,
       setInvers = setInvers,
       getInvers = getInvers)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
## checking whether the inverse is calculated or not
  invrs <- x$getInvers()
  if(!is.null(invrs)) {
    
    ## the inverse is already calculated to returning the cached value
    message("getting cached data")
    return(invrs)
  }

## inverse of matrix is not calculated so inversing the matrix and caching it
## getting data of original matrix in data variable through get() function
  data <- x$get()

  ## inversing the data through solve() function
  invrs <- solve(data, ...)

  ## caching the inversed matrix through setInvers() function
  x$setInvers(invrs)
  invrs
}
