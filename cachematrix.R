## makeCacheMatrix creates a special matrix which can cache its inverse. Please use the output of makeCacheMatrix
##as input for CacheSolve Function

## makeCacheMatrix creates a list of functions that can be applied on a matrix which are used in the next function in this
##script. set,get,setinverse,getinverse form a list of functions which are called appropriately in the next function

makeCacheMatrix <- function(x = matrix()) {
  inverse<- NULL
  set<-function(y){x<<-y
  m<<-NULL
  }
  get<-function() x
  setinverse<-function(i) inverse<<-i
  getinverse<-function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function decides if it knows the inverse of matrix x(output from makeCacheMatrix) and returns stored
##value otherwise it calculates inverse and stores it in setinverse
##example of using this: x<-matrix(c(3,4,5,7),nrow=2) 
## cacheSolve(makeCacheMatrix(x))


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

