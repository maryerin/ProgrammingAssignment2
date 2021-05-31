##These functions calculate and cache the inverse of a given matrix in the parent
##environment. 


## initialises x (default matrix object) and xinv (the object for storing the inverted matrix)
## and uses <<- to set them as objects in the parent environment rather than the local environment

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  #functions that will get or set the input and output matrices to objects in the parent environment
  #when cacheSolve is called
  set <- function(y){
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) xinv <<- solve
  getinv <- function() xinv
  #allows use of the $ operator to access the above functions
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Checks if the inverse has been stored previously, and if so, returns it.
## If not, calculates, stores, and returns xinv

cacheSolve <- function(x, ...) {
  #checks if xinv has been stored previously in makeCacheMatrix and if TRUE
  #returns it
  xinv <- x$getinv()
  if(!is.null(xinv)){
    message("getting cached data")
    return(xinv)
  }
  #if xinv hasn't been stored previously, calcs and stores it in makeCacheMatrix
  #then returns it
  data <- x$get()
  xinv <- solve(data)
  x$setinv(xinv)
  xinv
}
