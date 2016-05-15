## This is my week3 data science submission.
## The first function is creating a matrix, which will be cached. 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL  ## This is where the inverse of the matrix (m) is stored. 
  ## Set it to NULL if not calculated
  
  set <- function(y) {
    x <<- y
    m <<- NULL ## Define set 
}
  
  get <- function() x ## Get the input matrix
  setm <- function(inv) m <<- inv ## Set the inversed matrix to m
  getm <- function() m ## Return the inversed matrix
  
  list(set = set, get = get, setm = setm, getm = getm)
}
## Create the list of the 4 functions defined above
  

## This function will calculate the inverse of the matrix or fetch it from the cache.
cacheSolve <- function(x, ...) {
  
  m <- x$getm() ## Get the inversed matrix
  if(!is.null(m)) { ## if the inversion result is there 
    message("using cached data")
    return(m) ## use the cached inverse matrix
}
  data <- x$get() ## if it's not calculated yet, we get the original matrix
  m <- solve(data, ...) ## solve the matrix
  x$setm(m)
  m 
}
  ## print the output of the solved (inverse) matrix. 
  ## The previous 4 lines of code are skipped when the inverse matrix is in cache.
  


##the first time we solve the matrix, it will be calculated and stored to cache (no message)
##the second time the matrix is called to be solved, it will be fetched from cache. No calculations.
