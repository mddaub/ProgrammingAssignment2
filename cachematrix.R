##  This is a pair of functions that cache the inverse of a matrix.
##  makeCacheMatrix: This function creates a special "matrix" object 
##      that can cache its inverse.
##  cacheSolve: This function computes the inverse of the special 
##      "matrix" returned by makeCacheMatrix above. If the inverse 
##      has already been calculated (and the matrix has not changed), 
##      then the cachesolve should retrieve the inverse from the cache.


##  makeCacheMatrix creates a special "matrix", which is really a list 
##  containing a function to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse
##      4. get the value of the inverse

makeCacheMatrix <- function(X = matrix()) {
    InvX <- NULL
    set <- function(Y) {
        X <<- Y
        InvX <<- NULL
    }
    get <- function() X
    setInv <- function(solve) InvX <<- solve
    getInv <- function() InvX
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


##  cacheSolve calculates the inverse of the special "matrix" created 
##  with the above function. However, it first checks to see if the inverse 
##  has already been calculated. If so, it gets the inverse from the cache 
##  and skips the computation. Otherwise, it calculates the inverse 
##  and sets the value of the inverse in the cache via the setInv function. 

cacheSolve <- function(X, ...) {
    InvX <- X$getInv()
    if(!is.null(InvX)) {
        message("getting cached data")
        return(InvX)
    }
    data <- X$get()
    InvX <- solve(data, ...)
    X$setInv(InvX)
    InvX
}
