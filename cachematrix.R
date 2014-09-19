## the makeCacheMatrix is a function creates a matrix
## the cacheSolve function checks if there has been computed the inverse of the matrix, 
##   if so then simply prints the inverse, otherwise it computes the inverse and prints it

## Although it worked, I still don't grasp the <<- operator

makeCacheMatrix <- function(x = matrix()) {
     
     m<-NULL
     set<- function(y) {
          x<<-y
          m<<-NULL
     }
     get <-function() x
     setinverse <- function(solve) m <<- solve
     getinverse <-function() m
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function is a bit more straightforward

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m<-x$getinverse()
     if(!is.null(m)){
          message("getting inverse matrix")
          return(m)
     }
     data <- x$get()
     m<-solve(data,...)
     x$setinverse(m)
     m
}
