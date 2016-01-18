# makeCacheMatrix() function creates a special "matrix" object 
# that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL  #sets Inverse to NULL as a placeholder for future value
  
  # set() defines a function to set the matrix x to a new matrix y and 
  # resets the Inverse to NULL
  set<-function(y)
  {
    x<<-y
    Inv<-NULL
  }
  
  get <- function() x # returns the matrix x
  
  setInv <- function(Inv) Inv <<- solve # sets the value of Inverse to solve
  getInv <- function() Inv              # returns the value of the Inverse
  
  list(set=set,get=get,setInv = setInv,getInv = getInv)

}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.
cacheSolve <- function(x, ...) {
        
  Inv <-x$getInv() 
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInv(Inv)
  Inv
}
