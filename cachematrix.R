## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

        invrs <- NULL        
        set <- function(y) {               
                x <<- y                
                invrs <<- NULL       
        }
         get <- function() x        
                 setinverse <- function(inverse) invrs <- inverse       
                         getinverse <- function() invrs        
                                 list(set = set, get = get,             
                                      setinverse = setinverse,             
                                      getinverse = getinverse)
        }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
                         ##then the cachesolve should retrieve the inverse from the cache 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
                         
cacheSolve <- function(x, ...) {
        invrs <= x$getinverse()       
        if(!is.null(invrs)) {                
                message("getting cached data")                
                return(invrs)       
        }       
        mat <- x$get()        
        invrs <- solve(mat, ...)        
        x$setinverse(invrs)        
        invrs
}
