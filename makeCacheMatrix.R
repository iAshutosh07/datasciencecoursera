## Two functions created: makeCacheMatrix and cacheSolve.
## makeCacheMatrix caches a given matrix
## cacheSolve creates the inverse of a given matrix, 
## either by solving or recalling from cache

makeCacheMatrix <- function(x= matrix()){
    inv <- NULL ##i is the inverse matrix, to be created below
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    
    ## assigns matrix value
    get <- function() {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    
    ## sets and gets inverse of matrix
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

cacheSolve <- function(x, ...){
    inv <- x$getInverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
