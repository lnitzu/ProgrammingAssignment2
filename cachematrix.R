## Put comments here that give an overall description of what your
## functions do

## Function in charge of caching the object

makeCacheMatrix <- function(k = matrix()) {
  
  m<- NULL
  
  
  ## ===== setter======
  
  set <- function (y){
    k<<-y
    m<<- NULL
  }
  
  
  ##===== getter =====
  get <- function () k
  
  setInverse <- function(inverse) m<<- inverse
  
  getInverse <- function() m
  
  list (set = set, get=get, setInverse= setInverse, getInverse=getInverse)




}



## Checks if cache exists, if yes, delivers the object from cache, if not create

cacheSolve <- function(k, ...) {
  ## Return a matrix that is the inverse of 'x'

  m <- k$getInverse()
 
  
  if (!is.null(m)){
    message ('getting from the chache')
    return (m)
  }
  
  data<- k$get()
  m<-solve(data,...)
  k$setInverse(m)
  m        
        
}
