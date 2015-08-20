## Since matrix inversion is usually a costly calculation, 
## it might be beneficial to cache the inverse of a matrix 
## rather than calculate it repeatedly. 
## That is why, the following pair of functions make  
## the process of getting the inverse of a matrix more efficient
## by introducing the assignment operator <<- that enables to create
## a special object which stores a matrix and caches the inverse of it. 

#The function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(x=matrix()){
  ## Return a matrix that is the inverse of 'x'
  xinv<-NULL
  set<-function(y){
    x<<-y
    xinv<<-NULL
  }
  get<-function()x
  setxinv<-function(inv)xinv<<-inv
  getxinv<-function()xinv
  list(set=set, get=get, setxinv=setxinv,getxinv=getxinv)
  
}

## The function calculates the inverse of the special "matrix"
## returned by the aforementioned makeCacheMatrix function. 
## When the inverse has already been computed and the matrix has not
## been changed, then the cachSolve should get the inverse from the cache.

cacheSolve<-function(x,...){
  z<-x$getxinv()
  if(!is.null(z)){
    message("getting cached data")
    return(z)
  }
  data<-x$get()
  z<-solve(data)
  x$setxinv(z)
  z
}


