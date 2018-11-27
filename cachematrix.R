# the first function, makeCacheMatrix takes a matrix and gives a
# list containing a set of functions.This is taken as an input for cacheSolve,which
# computes the inverse of previously entered matrix

## The first function takes a matrix input and return a list containing a function which:
# 1.sets the matrix value
# 2.gets the matri value
# 3.sets the inverse value
# 4.gets the inverse value

makeCacheMatrix <- function(x = matrix()) {
  xInv<-matrix(0)
  set<-function(y){
    x<<-y
    xInv<<-matrix()
  }
  get<-function() x
  setInverse<-function(solve) xInv<<-solve
  getInverse<-function() xInv
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  
}


## cacheSolve takes the list of function and calculates the matrix inverse.
# If the inverse is calculated, it will return the cached value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xInv<-x$getInverse()
  if(!is.na(xInv)){
    message("getting cache data")
    return(xInv)
  }
  data<-x$get()
  xInv<-solve(data, ...)
  x$setInverse(xInv)
  xInv
}
