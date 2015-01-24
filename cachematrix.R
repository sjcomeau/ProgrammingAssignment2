## cachematrix.R
## 1/24/2015
##  These functions are used to store a square matrix and 
##  calculate and cache the inverse of the square matrix.

##Steps for using these functions:
## 1. Make a two square matrix using: 
##              sm <- matrix(c(1,2,5,6,7,8,9,10,12), nrow=3, ncol=3)
##              sm2 <- matrix(c(10,20,50,60,70,80,90,100,120), nrow=3, ncol=3)
## 2. Run the makeCacheMatrix() to create a list of functions.
##              invfun.list <- makeCacheMatrix(sm)
## 3. Run invfun.list to see list of functions.
## 4. Run invfun.list$get() to get the array that was set in step 2 above.
## 5. Run invfun.list$getinverse() and get NULL.
## 6. Run cacheSolve(invfun.list) and cacl inverse and get inverse of matrix.
## 7. Run cacheSolve(invfun.list) again: 
##              see message: "getting cached data" and get inverse of matrix
## 8. Run sequence again, using sm2


## Creates a list of 4 functions, which act to hold and transfer objects: 
##      set(y), get(), setinverse(inv), getinverse()
##  1. Function set(y) is used to set the matric object into local object x.
##     The object i is set to NULL, to clear the associated inverse.
##     (this also occurs when first creating the fucntion list)
##  2. Function get() returns local object x, the matrix.
##  3. Function setinverse(inv) is called from cacheSolve() with 
##     and passes in a calculated matrix inverse.
##     The  matrix inverse is then stored in local object i.
##  4. Function getinverse returns local object i.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## When first run calculates, saves, and returns inverse of square matrix passed in.
## When run again, returns the cached inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        ##i <- solve(data, ...)
        i <- solve(data)
        x$setinverse(i)
        i
}

