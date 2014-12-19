## The functions calculate the inverse of the matrix efficiently by
## caching the inverse matrix for future references
## 

## This matrix caches the matrix and its inverse and has the getter-setter
##functions for the matrix and its inverse.


makeCacheMatrix <- function(mat = matrix()) {
  
  inv <- NULL
  
  set <- function (y){
    mat <<-y
    inv <<-NULL
  }
  get <- function() mat
  
  setinverse <- function (yinv){
    inv <<-yinv
  } 
  
  getinverse <- function() inv
  
  list(set=set, get = get, 
       setinverse = setinverse ,
       getinverse = getinverse)
  

}

## This method computes inverse if not already present in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
  inv <- x$getinverse()
  
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  inv <- solve(x$get())
  
  x$setinverse(inv)
  
  inv       
  
}
