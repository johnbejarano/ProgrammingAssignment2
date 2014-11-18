## These two functions, makeCacheMatrix, and cacheSolve work together to store
## a matrix and a cached inverse of that matrix.  These functions were
## developed to demostrate aspects of lexical scoping for Programming
## Assignment #2 in the R Programming class within the Data Science
## Specialization at Johns Hopkins University taught through Coursera.

## Author:  John Bejarano

## This first function, makeCacheMatrix, produces an object that stores a matrix,
## returns the value of that matrix, stores the inverse of that matrix (as a
## cache), and returns the value of that inverse matrix.  The object has four
## methods:

## "set" stores the original "non-inversed" matrix value.  (The original call
## of this function can also set this value.)

## "get" returns the value of this original matrix.

## "setinv" stores a cache of the inverse of the original matrix.  This will
## typically be called by the cacheSolve function below.

## "getinv" returns the cached value of the inverse matrix.

makeCacheMatrix <- function(x = matrix(c(1,0,0,1),nrow = 2, ncol = 2)) {
        
        mxinv <- NULL                   #Initialize inverse matrix.
        
        set <- function(newmx) {        #Function to change current matrix.
                x <<- newmx             #Replace current matrix with new matrix.
                mxinv <<- NULL          #Reset inverse matrix to NULL since original
                #matrix no longer applies.
        }
        
        get <- function() {
                x                       #Return current matrix.
        }                               
        
        setinv <- function(newinv) {    #Function to store inverse matrix.
                mxinv <<- newinv        #Replace current inverse matrix with new
                #inverse matrix.
        }
        
        getinv <- function () {
                mxinv                   #Return current inverse matrix.
        }
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This second function, cacheSolve, returns the inverse of the original matrix
## stored in the makeCacheMatrix function object.  If that inverse matrix has
## already been calculated and cached, it simply returns that cached inverse
## matrix.  If the inverse matrix has not been calculated and cached yet, it
## calculates the inverse matrix, and stores it in the makeCacheMatrix function
## object for future calls.

cacheSolve <- function(x, ...) {
        mxinv <- x$getinv()             #Retrieve cached inverse matrix if available.
        
        if(!is.null(mxinv)) {
                message("displaying cached inverse matrix")
                return(mxinv)           #Display the cached inverse matrix.
        }
        
        #The following code will only be executed
        #if no inverse matrix is cached.  The code
        #will provide a cached inverse matrix for
        #future runs.
        
        mxdata <- x$get()               #Get the current matrix's data.
        mxinv <- solve(mxdata, ...)     #Calculate the inverse of the current matrix.
        x$setinv(mxinv)                 #Store the just-calculated inverse matrix in cache.
        mxinv                           #Display this just-calculated inverse matrix.
}
