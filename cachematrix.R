## Since matrix inversion is usually a costly calculation, 
## it might be beneficial to cache the inverse of a matrix 
## rather than calculate it repeatedly. "

## That is why, the following pair of functions make  
## the process of getting the inverse of a matrix more efficient
## by introducing the assignment operator <<- that enables to create
## a special object which stores a matrix and caches the inverse of it. 


#The function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(x=matrix()){
 xinv<-NULL 
 set<-function(y){
    x<<-y
    xinv<<-NULL   #stores the inverse of matrix in cache 
 }
 get<-function()x                                        #gets a matrix
 setxinv<-function(solve)xinv<<-solve                    #sets the inverse of matrix 
 getxinv<-function()xinv                                 #gets the inverse of a matrix
 list(set=set, get=get, setxinv=setxinv,getxinv=getxinv) #creates a list of functions
 }

## The function calculates the inverse of the special "matrix"
## returned by the aforementioned makeCacheMatrix function. 
## When the inverse has already been computed and the matrix has not
## been changed, then the cachSolve should get the inverse from the cache.

cacheSolve<-function(x,...){
  ## Return a matrix that is the inverse of 'x'
  xinv<-x$getxinv()                #asks for cache of x matrix
  if(!is.null(xinv)){              #if a cache exists the inverse of the matrix was calculated
    message("getting cached data") #shows the message
    return(xinv)                   #returns the cache
  }
  data<-x$get()                    # if not, the matrix object is got 
  xinv<-solve(data,...)            #computes the inverse of the matrix
  x$setxinv(xinv)                  # the inverse is set to the object
  xinv                             #returns the solved result
}


