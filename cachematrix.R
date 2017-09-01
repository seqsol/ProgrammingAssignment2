## Put comments here that give an overall description of what your
## functions do

## The functions compute and cache the inverse matrix of a given matrix 
## so that it will not be computed again when it is needed 

## Write a short comment describing this function

## The function receives a matrix as its input and return a list of 
## four functions that can cache the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set <- function(y) {
        x<<-y
        inv<<-NULL
    }
    get <- function () x
    setinv <- function(inv_m) inv <<- inv_m
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

## This function computes the inverse of the special matrix returned from 
## above function. If the inverse already exists, then the function will just 
## retrive it from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached inverse matrix")
        return(inv)
    }
    else{
        m <- x$get()
        inv <- solve(m, ...)
        x$setinv(inv)
        inv
    }
}
