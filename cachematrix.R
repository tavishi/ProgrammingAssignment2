##The functions cache the inverse of a matrix in order to prevent costly recalculation of inverse evertytime it is invoked

## The makeCacheMatrix() function creates a special matrix object and closure functions to set and get matrix, set and get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        
  inv<-NULL
  
  setmatrix<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  getmatrix<-function(){x}
  
  setinverse<-function(i){inv<<-i}
  
  getinverse<-function(){inv}
  
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
  
}


##The cacheSolve function returns the inverse of matrix if previously calculated and saved in the cache 
## and in case the inverse has not been calculated previously it calculates the inverse and saves it in the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversecache<-x$getinverse()
  
  if(!is.null(inversecache)){
    message("Getting cached data")
    return(inversecache)
  }
  
  m<-x$getmatrix()
  inversecache<-solve(m,...)
  x$setinverse(inversecache)
  
  inversecache
  
}
