##The following two functions are used to create a special object 
##that it stores a matrix and cache's its inverse.

##This function makeCacheMatrix creats a special"matrix",
##which is to get the value of the inverse matrix

makeCacheMatrix<-function(x=matrix()){
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinv<-function(solve) i<<-solve
  getinv<-function() i
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}

##This function is used to calculate the inverse of the special "matrix"

cacheSolve<-function(x, ...){
  i<-x$getinv()
  if(!is.null(i)){
    message("getting cached matrix")
    return(i)
  }
  matrix<-x$get()
  i<-solve(matrix, ...)
  x$setinv(i)
  i
}
