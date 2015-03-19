## makeCacheMatrix takes the argument x which is a matrix
## that the user wants to compute the inverse of. cacheSolve
## allows the user to call upon the inverse of x. If the 
## inverse has already been computed, cacheSolve retrieves
## the inverse from memory. If the inverse has not been
## previously computed, cacheSolve computed the inverse
## and then caches it in memory through the setinv function
## in makeCacheMatrix.

## The result of makeCacheMatrix is a list of functions
## that can be called in relation to x in order to
## set x, retrieve x, set the inverse of x, or
## retrieve the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      
      set <- function(x) {
            x <<- x
            i <<- NULL
      }
      
      get <- function( ) x 
      ##calling get() returns the matrix x
      
      setinv <- function(inv) i <<- inv
      ##setinv allows you to store the inverse 
      ##(that you call as the argument) for later use
      
      getinv <- function( ) i 
      ##calling getinv() returns the cached inverse
      
      list(set = set, get = get, 
           setinv = setinv,   
           getinv = getinv)   
}


## If the inverse of a matrix x has already been calculated
## then this function will retireve the inverse from the 
## makeCacheMatrix function. If the inverse has not yet been
## calculated, cacheSolve will calculate the inverse and then
## cache the inverse for later use.

cacheSolve <- function(x, ...) {
        i <- x$getinv()  
        if(!is.null(i)){ 
                message ("getting cached inverse")
                return (i)
        } else if(is.null(i)){ 
                message ("computing inverse and caching") 
            
                data <- x$get()
                i<-solve(data)
                x$setinv(i) 
            
                return(i)
        }
}
