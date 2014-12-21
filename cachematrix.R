## The below functions check to see if the inverse of a matrix is 
## cached. If so they return the cached value. If not they calculate
## the inverse.

## This function creates a special list of function definitions
## that are used by the cacheSolve functions to set and/or get the 
## a matrix object and its inverse

makeCacheMatrix <- function(x = matrix()) {
   inv<-NULL
   set<-function(y){
      x<<-y
      inv<<-NULL
   }  ## sets the matrix object and clears any previously calculated 
      ## inverse from the cache 
   get<-function() x    ## returns the current matrix object
   setinv<-function(inverse) inv<<-inverse ## chache inverse of the matrix
   getinv<-function() inv ## return the cached inverse
   list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function first checks to see if the inverse of the matix is 
## cached. If so, it returns the inverse. If not, it  

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   
   inv<-x$getinv()  ## Check to see if the inverse has already been calculated
   if (!is.null(inv)) {
      ## return the cached value of the inverse
      message("getting cached data")
      return(inv)
   } 
   ## calculate the inverse
   message("calculating inverse of matrix")
   mat<-x$get()      ## get the matrix
   inv<-solve(mat)   ## calculate the inverse
   x$setinv(inv)     ## cache the inverse
   inv               ## return the calculated inverse
}
