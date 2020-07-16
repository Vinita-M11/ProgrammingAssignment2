## This is a pair of functions in which, the first one creates a list of functions which set and get values (of the matrix and of the inverse) and the second one calculates the inverse of the given matrix and caches that.If the inverse ,however ,is already calculated it returns the cached inverse

## The makeCacheMatrix function just provides a list of functions useful for setting and calling data. 
## When we pass the required data to this function it stores a list of the 4 functions corresponding to the provided data

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){x<<-y
  i<<-NULL}
  get<-function(){x}
  setinverse<- function(inv){i<<-inv}
  getinverse<-function() {i}
  list(set= set,get= get,setinverse=setinverse,getinverse=getinverse)
}



## CacheSolve first checks if the value of inverse is cached and if so it skips the computation and returns the cached value. 
## Otherwise it calculates the inverse and in turn caches that value.Thus if the same data (which has been created using the makeCacheMatrix function) is provided twice to this function it will calculate the inverse the first and the next time it'll just return the cached value which will save time.


cacheSolve <- function(x, ...) {
    i<-x$getinverse()
    if (!is.null(i)){
      print("Getting your cached inverse")
      return(i)
    }
    matrix<-x$get()
    i<-solve(matrix)
    x$setinverse(i)
    i

}
