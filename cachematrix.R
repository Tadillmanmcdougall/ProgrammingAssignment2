################################################################################
##
##  Author:  Tristan Dillman McDougall
##  Institution:  UC Berkeley
##  Project ID:  System_Documentation
##  Proj Purpose: Document system configuration at project inception
##  File Purpose: Document system configuration in text format
##  Date:  7/25/2016
##
##
################################################################################
##
##  System configuration
##  Windows 10 Home 64 bit, AMD A10-8700P Radeon R6 1.8 GHz, 6.00 GB
##  Version control:  git scm 2.7.2
##
################################################################################
##
## R configuration
## R x64 3.2.4
## RStudio Version 0.99.892
##
################################################################################
##
## Revision History
##
##      7/24/16:  initial clone of GitHub Repository
##      7/25/16:  coded makeCacheMatrix & cacheSolve              
##
################################################################################
##
## Put comments here that give an overall description of what your
## functions do
##
##  Methdology:
##  (1)    Create object that contains matrix and its inverse using the
##         super assignment operator <<-
##  (2)    Create object that calculates inverse of matrix and stores
##         that inverse within the initial matrix object.
##      
##

## Write a short comment describing this function

## Function creates a closure that contains setters and getters for a 
## matrix and its inverse. 
## set allows user to change the matrix being stored and
## eliminates inverse of the previously stored matrix.
## get returns stored matrix.
## setinverse assigns the input value (inverse) to inverseX which is intended to
## store the inverse of the stored matrix. 
## getinverse returns stored inverse matrix
##
##              
## For more detail regarding lexical scope, INCLUDING the superassignment
## operator <<-,
## see http://cran.r-project.org/doc/manuals/r-release/R-intro.html#Scope
## (Section 10.7 Scope)

makeCacheMatrix <- function(x = matrix()) {
    inverseX <- NULL
    set <- function(y) {
        x <<- y
        inverseX <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inverse) {inverseX <<- inverse}
    getinverse <- function() {inverseX}
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function first determines if an inverse to the stored matrix exists. If an
## inverse does not exist, then the function will solve for the inverse. After,
## the function uses the inverse setter (inverseset) from "makeCachematrix" to
## store the inverse matrix in memory. Finally, the function returns the inverse
## matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseX <- x$getinverse()
    if (!is.null(inverseX)) {
        message("getting cached data")
        return(inverseX)
    }
    matrix <- x$get()
    inverseX <- solve(matrix, ...)
    x$setinverse(inverseX)
    inverseX
}

#### Examples for usage of these closures
## TestMatrix <- makeCacheMatrix(diag(2, 3000, 3000))
## TestMatrix$get()
## TestMatrix$getinverse()
## cacheSolve(TestMatrix)
## cacheSolve(TestMatrix)
#### Second itteration happens very quickly