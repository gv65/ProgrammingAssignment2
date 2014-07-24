## Put comments here that give an overall description of what your
## functions do
## This program compute the Inverse Matrix and cache it. Inversion is usually a costly computation
## and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

## Write a short comment describing this function

## makeCacheMatrix : This function creates a special matrix object that can cache it inverse
## It is a matrix containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
	x <<- y
	m <<- NULL
}
get <- function() x
setInvMatrix <- function(solve) m <<- solve
getInvMatrix <- function() m
list(set=set, get=get,
   setInvMatrix = setInvMatrix,
   getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function

## cacheSolve : This function computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),then the cacheSolve 
## should retrieve the inverse from the cache.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse matrix in the cache via 
## the setInvMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInvMatrix()
    if(!is.null(m)){ 
		message("Getting cached data ........")
		return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setInvMatrix(m)
    m
}

## Execution Results
## -----------------
## Data set I
## ----------
## > a <- rbind(c(1,0,0),c(0,1,0),c(0,0,1))
## > m <- makeCacheMatrix(a)
## > m$get()
##      [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1

## Initially, no cache. So in the first run Inverse matrix is computed
## > cacheSolve(m)
##     [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1

## Data set II
## -----------
## > b <- rbind(c(10,20,30),c(20,30,10),c(30,10,20))
## > m1 <- makeCacheMatrix(b)
## > m1$get()
##     [,1] [,2] [,3]
## [1,]   10   20   30
## [2,]   20   30   10
## [3,]   30   10   20

## Even though cache is available, but the matrix is different. So Inverse matrix is computed
## > cacheSolve(m1)
##             [,1]         [,2]         [,3]
## [1,] -0.027777778  0.005555556  0.038888889
## [2,]  0.005555556  0.038888889 -0.027777778
## [3,]  0.038888889 -0.027777778  0.005555556

## Consecutive cacheSolve calls for m and m1, the inverse matrices are retrieved 
## from cache.

## > cacheSolve(m)
## Getting cached data ........
##     [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1

## > cacheSolve(m1)
## Getting cached data ........
## > cacheSolve(m1)
##             [,1]         [,2]         [,3]
## [1,] -0.027777778  0.005555556  0.038888889
## [2,]  0.005555556  0.038888889 -0.027777778
## [3,]  0.038888889 -0.027777778  0.005555556