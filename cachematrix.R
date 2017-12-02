## The program is composed of two functions:
## (1) makeCacheMatrix is a function that creates the matrix object x.
##     This also has the ability to cache the matrix' inverse
## (2) cacheSolve computes the inverse of the matrix object.
##     If the inverse has alreadybeen calculated, then it will just retrieve
##     the output from makeCacheMatrix function
## There is an observable improvement in the execution of the code when the value of
## the inverse matrix has been cached. It doesn't recalculate the value one's it has been calculated.

#matlib package was installed to enable the use of "inv" function
# install.packages("matlib")
# library(matlib)

## makeCacheMatrix stores the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  matinv<-NULL
  set<-function(y){
    x<<-y
    matinv<<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) matinv <<- inverse
  getinverse <- function() matinv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calculates the inverse of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  k <- x$getinverse()
  if(!is.null(k)) {
    message("getting cached data")
    return(k)
  }
  data <- x$get()
  k <- inv(data,...) #inv function calculates the inverse of the input data (matrix).
  x$setinverse(k)
  k
}
