
    ## "makeCacheMatrix" creates a special matrix, which is really a list 
    ## containing functions to
    ## 1.  set the value of the matrix
    ## 2.  get the value of the matrix
    ## 3.  set the value of the matrix inverse
    ## 4.  get the value of the matrix inverse
    
    ## "cacheSolve" first checks to see if there had been any matrices cached
    ## for the inverse, and if there had been, it returns that matrix.
    ## If no matrices had been chaced, it calculates the inverse and sets the
    ## cache via the setinverse function

makeCacheMatrix <- function(x = matrix()) {    
    z <- NULL
    set <- function(y)      {
        x <<- y
        z <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) z <<- solve
    getinverse <- function() z
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

cacheSolve <- function(x, ...) {
    z <- x$getinverse()
    if(!is.null(z)) {
        message("getting cached data")
        return(z)
    }
    data <- x$get()
    z <- solve(data, ...)
    x$setinverse(z)
    z
}
