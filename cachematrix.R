## The following functions create a inverse of a matrix, the first
## creates a cache of the matrix and inverse, the second calculates the 
## inverse or calls the cached matrix if one already exists.


## this function creates a list contining functions to:
##set the matrix, get the matrix, set the inverse and
## get the inverse

makeCacheMatrix <- function(x = matrix()) {
    i<- NULL
    set <- function(y){
            x<<-y
            i<<-NULL
    }
   get<- function()  x
   setinverse <- function(inverse) i<<-inverse 
   getinverse <- function() i
   list(set = set,get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
}


## This function checks to see if a inverse already exists
## for this matrix if so it retieves the cache otherwise
## it calculates the inverse of the matrix, sets the 
## to the cache and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        if(!is.null(i)){
            message("Getting cached inverse")
            return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setinverse(i)
        i
}
