## Submission for Module 2 (Assignment 2 - R Programming)
## By David Hayes - August 2014


makeCacheMatrix <- function(x = matrix()) { # input x is a matrix
  ##
  ## makeCacheMatrix: This function creates a special "matrix" 
  ## object that can cache its inverse.
  ##
  i <- NULL               			            # i is the inverse and set to NULL when makeCacheMatrix called
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x                       # returns value of original matrix
  setinverse <- function(solve) i <<- solve # stores value using super-assignment
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
} 



cacheSolve <- function(x, ...) {            # the input is an object created by makeCacheMatrix
  ##
  ## cacheSolve: This function computes the inverse of
  ##  the special "matrix" returned by makeCacheMatrix above.
  ## If the inverse has already been calculated
  ## (and the matrix has not changed), then the cachesolve should return
  ## the inverse from the cache (i..e . the inverse of 'x')
  ##
  i <- x$getinverse()                       # accesses the object x and gets the value of inverse
  if(!is.null(i)) {                         # if inverse was already cached (i.e. not NULL) ...
    message("getting cached data")          # sends message to console
    return(i)   # returns inverse
  }
  data <- x$get()                           # if not already cached then ...
  i <- solve(data, ...)                     # have to calculate the inverse
  x$setinverse(i)                           # store inverse
  i  		                                    # returns the inverse
}  