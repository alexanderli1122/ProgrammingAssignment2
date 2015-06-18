  ## The following functions (1) take a square matrix, and calculates its inverse matrix
  ## in its cache. (2) Then the inverse matrix will be called from its cache and returned.
  
  ## caches inverse matrix x
  
  makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
      x<<-y
      m<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) m <<-solve
    getinverse<-function() m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  }
  
  
  ## return inverse of matrix x from cache
  
  cacheSolve <- function(x, ...) {
    m<-x$getinverse()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinverse(m)
    m
  }
