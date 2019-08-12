
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## ---------------------------------------------------------------------------------------------
## For a given (not singular) matrix A, a special list/vector is created, consisting of the functions set, get, setAinv, getAinv
## ---------------------------------------------------------------------------------------------

makeCacheMatrix <- function(A = matrix()) {
        Ainv <- NULL  
        set <- function(y) {
                A <<- y 
                Ainv <<- NULL
        }
        get <- function() A  
        setAinv <- function(solv) Ainv <<- solv  
        getAinv <- function() Ainv  
        list(set = set, get = get,
             setAinv = setAinv,
             getAinv = getAinv) 
}



## Write a short comment describing this function
## ---------------------------------------------------------------------------------------------
## For a given (not singular) matrix A, the inverse Ainv is calculated and cached - unless it was already calculated
## ---------------------------------------------------------------------------------------------

cacheSolve <- function(A=matrix(), ...) {
        Ainv <- A$getAinv()
        if(!is.null(Ainv)) {
                message("getting cached data")
                return(Ainv)
        }
        data <- A$get()
        Ainv <- solve(data, ...)
        A$setAinv(Ainv)
        Ainv
}