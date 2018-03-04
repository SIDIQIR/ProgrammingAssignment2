## the makeCacheMatrix function creates a special matrix and it contains a function list:

## set the value of the matrix
## get the value of the matrix
## set the value of the invers
## get the value of the invers
makeCacheMatrix <- function(x = matrix()) {
  
    Invmat <- NULL
    set <- function(y) {
          x <<- y
          Invmat <<- NULL
  }
  get <- function() x
  setinvers <- function(invers) Invmat <<- invers
  getinvers <- function() Invmat
  list(set = set, 
       get = get, 
       setinvers = setinvers,
       getinvers = getinvers)
}

## The function below computes the inverse of the special "matrix" returned by makeCacheMatrix from above function.
## First it tests if the inverse is completed, it gets the invers from cache and skips the computation. 
## If the inverse is not completed, it calculates the invers of the data 
## and sets the value to cache through setinvers function. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  Invmat <- x$getinvers()
  if(!is.null(Invmat)) {
        message("getting cached data")
        return(Invmat)
  }
  data <- x$get()
  Invmat <- solve(data, ...)
  x$setinvers(Invmat)
  Invmat
}
