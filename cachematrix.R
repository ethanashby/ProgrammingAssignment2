## These functions take a square matrix, invert it and store inverse 
## in cache

## makeCacheMatrix defines get and set functions and returns in a list

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
  	x <<-  y
  	m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set=set, get=get,
     setmatrix=setmatrix,
     getmatrix=getmatrix)
}


## cacheSolve checks if inverse stored in cache and if not computes 
## inverse and puts in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)){
    	message("getting cached data")
      return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
