## Makes cahce of matrix & checks if inverse exists in cache & if not, calculates inverse

## Makes a cache of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <-NULL
  set <- function (y){
    x <<- y
    inverse <<- NULL
    
  }
  get <- function()x
  setinverse <- function(x){
    inverse<<-x    
  }
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse = getinverse)
  
}


## checks if inverse exists in cache & if not, calculate inverse of matrix


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  dat <- x$get()
  inverse <- solve(dat, ...)        
  x$setinverse(inverse)
  inverse
  
}