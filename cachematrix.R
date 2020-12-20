
## Assignment 2 : R-Programming

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# "
# Caching the Inverse of a Matrix:
#         
# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly.
# 
# Below are a pair of functions that are used to create a special object that
# stores a matrix and caches its inverse.
# 
# This function creates a special matrix object that can cache its inverse.
# "


makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInv<- function(inverse) {inv <<- inverse}
        getInv <- function() {inv}
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
# "
# This function computes the inverse of the special 'matrix' created by
# makeCacheMatrix above. If the inverse has already been calculated (and the
# matrix has not changed), then it should retrieve the inverse from the cache.
# "

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("Getting cached data of the matrix ")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}

#Testing my program
# Example :
m <- matrix(sample(100:1,36),6,6) 
m1 <- makeCacheMatrix(m)
m1$get()
m1$getInv()
cacheSolve(m1)
cacheSolve(m1)



