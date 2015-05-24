## This R script "cachematrix.R" contains two functions i.e.
## makeCacheMatrix and cacheSolve as part of Programming Assignment 2

## 1. makeCacheMatrix creates a special matrix object that supports caching the
## inverse.

## 2. cacheSolve will retrieve the cache of makeCacheMatrix if it has already
## been calculated previously and has not changed. If inverse is not created
## it will calculate the inverse of x.


## This function contains a list of 4 functions that will help set/get a matrix
## or set/get an inverse matrix. This is to support the caching the inverse
## matrix. Structure is fairly similar to makevector in example code
makeCacheMatrix <- function(x = matrix()) {

        inv_x <- NULL  #clears inv_x
        
        setmatrix <- function(new_matrix) {   # sets new matrix
                
        # Sets new matrix if only new_matrix is different from previous matrix        
                if (!identical(x,new_matrix)) {
                        x <<- new_matrix
                        ## x has been changed, therefore inverse of x has to be
                        ##  cleared for future inverse x matrix computation
                        inv_x <<- NULL
                }
                
        }        
        # getmatrix is a function that returns x matrix
        getmatrix <- function() x
        
        setinversematrix <- function(inv_matrix) { #sets new inverse matrix
                inv_x <<-inv_matrix
        }
        getinversematrix <- function() inv_x # function to get inverse of x matrix
        
        # returns a list of 4 functions
        list (setmatrix = setmatrix, getmatrix = getmatrix,
              setinversematrix = setinversematrix,
              getinversematrix = getinversematrix)
}


## This function will return the cache inverse matrix created by makeCacheMatrix
## as defined above. It will only return a cache inverse if it has been
## calculated previously and matrix have not changed. This is done by checking
## if inv_x has already been calculated (no NULL value). Otherwise it will
## compute the inverse and set the inv_x using the setinversematrix function.

cacheSolve <- function(x, ...) {
        #Gets the inverse matrix cache
        inv_x <- x$getinversematrix()
        
        #if inv_x is not null i.e. cached previously then
        if (!is.null(inv_x)) {    
                message("retrieving cached inverse matrix")
                return(inv_x)     #exits and returns cache inverse matrix
        }
        
        #Get data of matrix since cache of inverse matrix does not exist
        data <- x$getmatrix()      
        inv_x <- solve(data)            #computes inverse of acquired matrix
        x$setinversematrix(inv_x)       #caches the inverse matrix
        inv_x                      
        ## Return a matrix that is the inverse of 'x'
}

## How to test that both functions work?
## =====================================
## I have given the following examples of interactive code with script to prove
## that it is running correctly:

## Before you start please, ensure script is stored in your working directory

## > rm(list=ls())              # Clears workspace
## > source('cachematrix.R')      # Loads the script
## > a=matrix(c(1,2,3,4),2,2)   # Creates the 2x2 matrix
## > x <- makeCacheMatrix()
## > x$setmatrix(a)             # Sets the matrix with "a"
## > x$getmatrix()              # Check matrix is set correctly 
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > y <- cacheSolve(x)         # Notice that there is no message so this is a
                                # new inverse matrix computation.
## > y                          # Print inverse matrix
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Test #1 to multiply matrices a and y which returns correct identity matrix
## below to proof inverse function works

## > a %*% y            
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1

## Test 2 to set "a" same matrix as the inverse of x. Solving it should return
## back to original matrix of "a"

## > a <- y             
## > x$setmatrix(a)             # set new matrix
## > y <- cacheSolve(x)         # new inverse matrix is calculated
## > y                          # Original "a" matrix is shown
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > y <- cacheSolve(x)         # Running it a second time shows it is 
## retrieving cached inverse matrix
##
## ---- END OF EXAMPLE ----