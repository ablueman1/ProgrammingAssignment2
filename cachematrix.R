## The functions below cache the inverse of a matrix.
## This allows one to save time when implementing
## a program that requires matrix inversion, which 
## is typically a costly process with regard to time.

## The code written below follows closely the 
## "Caching the Mean of a Vector" example given
## in the Programming Assignment 2 Instructions. 

##
## The makeCacheMatrix function creates a special
## "matrix" object that can cache its inverse.
## In particular, makeCacheMatrix creates a list
## containing functions that:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the associated inverse matrix
## 4. get the value of the associated inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        
        # default inverse to null
        inv <- NULL
        
        # setting matrix value (1. above)
        set <- function(y){    
                x <<- y        # caching input for cacheSolve
                inv <<- NULL   # sets inverse to null if cacheSolve used
        }
        
        # returns input matrix (2. above)
        get <- function() x    
        
        # sets inverse of input (3. above)
        setinv <- function(inverse) inv <<- inverse
        
        # returns inverse of input (4. above)
        getinv <- function() inv  
        
        # create a list of the functions above
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function cacheSolve determines if the inverse of the argument
## matrix has already been computed and cached. If so, this function
## recovers and returns the cached result, with a message indicating
## that it has done so. If not, this function computes and caches the 
## inverse of the argument matrix.

cacheSolve <- function(x, ...) {
        
        # assigns inverse of 'x' to 'inv'.
        # NULL is assigned if inverse is not yet computed
        inv <- x$getinv()
        
        # if inverse is not NULL, return the previously calculated inverse
        if(!is.null(inv)){
                message("Getting Cached Matrix")
                return(inv)
        }
        
        # if inverse is NULL, get the matrix ... 
        data <- x$get()
        
        # compute the inverse ...
        inv <- solve(data, ...)
        
        # set the inverse ...
        x$setinv(inv)
        
        # return the inverse
        inv
}

## The following is an example of what these functions do.
## Note that the second 'cacheSolve' lets the user
## know that the inverse has already been cached.
## We use a 2x2 elementary matrix for clarity.
##
## > x <- makeCacheMatrix(rbind(c(1,1),c(0,1)))
## > x$get()
##      [,1] [,2]
## [1,]    1    1
## [2,]    0    1
## > cacheSolve(x)
##      [,1] [,2]
## [1,]    1   -1
## [2,]    0    1
## > cacheSolve(x)
## Getting Cached Matrix
##      [,1] [,2]
## [1,]    1   -1
## [2,]    0    1
