#The function will accept a matirx and cache inverse
#by involing subsequent functions

makeCacheMatrix <- function(x = matrix()) {
  #To store the inverse
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function (inverse) inv <<- inverse
  getInverse <- function () inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

#Calculates the inverse from the cache if available else
#re-calulates the value and sets it back to the cache
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Data from the cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv        
}
