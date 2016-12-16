## The following functions create an inverse of a matrix and cache it 
## to avoid calculating the inverse multiple times. 
## The first function creates a cache of the matrix and its inverse, 
## the second function calls the cached inverse matrix if it exists or  
## calculates the inverse if it does not already exist.


## This first function creates a list contining functions to:
##set the matrix, get the matrix, set the inverse of the matrix &
## get the inverse of the matrix.

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


## This second function checks in the cache to see if 
## an inverse already exists for this matrix, if so it 
## retruns it otherwise it calculates the inverse,
## sends the result to the cache so it won't be calculated 
## again and then returns the inverse matrix.
## 

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
