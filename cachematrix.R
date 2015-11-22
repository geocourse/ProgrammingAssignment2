## [The above functions cache the inverse matrix of a given matrix]

## [The first function:
## 1.sets the value of the matrix
## 2.gets the value of the matrix
## 3.sets the inverse matrix values and finally
## 4.gets the inverse matrix values]


makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) invm <<- solve
  getmatrix <- function() invm
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}

## [The second function:
## 1.checks if the inverse matrix has already been computed
## 2.if yes, it gets the inverse matrix values already computed
##   if not, it computes the inverse matrix 
##   and sets the values in the cache via the setmatrix function]


cacheSolve <- function(x, ...) {
  
  invm <- x$getmatrix()
  if(!is.null(invm)) {
    message("getting cached inverse matrix")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setmatrix(invm)
  invm
}
 
 ## Return a matrix that is the inverse of 'x'

