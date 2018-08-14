## The function 'makeCacheMatrix' creates a special matrix
## with a list of additional functions
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

## Example:

## 1. Create a matrix
##
## A <- matrix( c(5, 1, 0,
## 3,-1, 2,
## 4, 0,-1), nrow=3, byrow=TRUE)

## 2. Load the source file
## source("cachematrix.R")
## 
## 3. Create the special matrix
## 
## cm_A <- makeCacheMatrix(A)

## 4. call cacheSolve - first run
## cacheSolve(cm_A)
## this will cache and return the inverse

## 5. call cacheSolve - second run
## cacheSolve(cm_A)
## this will output the value from cache with a 
## message indicating its from cache


## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Returns a cached inverse value of a "special matrix" if present
## if not, then it caches the value and returns the same next time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
