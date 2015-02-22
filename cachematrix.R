## makeCacheMatrix() initializes everything.  Supply with the matrix.
## Then use cacheSolve() to get the actual inverse.
## e.g.
##      mat <- matrix(ceiling(runif(100,0, 100)), 10, 10)  # 10x10 matrix of ints
##      foo <- makeCacheMatrix(mat)
##      invMat <- cacheSolve(foo)
##      invMat2 <- cacheSolve(foo)  # from cache
##      identity <- mat %*% invMat  # check answer
##      round(identity, digits=1)   # check it.
##      round(mat %*% invMat2, digits=1) # check from cache.

## Make a list of functions for storing and retrieving matrix inverses
## from a cache.

makeCacheMatrix <- function(x = matrix()) {
        ## x       is the matrix data
        ## matInv  is the matrix inverse
        matInv <- NULL
        set <- function(y) {
                x <<- y
                matInv <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) matInv  <<- inv
        getInverse <- function() matInv
        ## make list of functions.
        list(set = set, get = get,
             setInverse = setInverse ,
             getInverse = getInverse )

}


## Get the inverse of a matrix.
## If the inverse has been cached, retrieve from cache.
## Otherwise, compute the matrix inverse and store in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matInv <- x$getInverse ()
        if(!is.null(matInv )) {
                ## Found inverse in cache.  Just return, don't calc.
                message("getting cached data")
                return(matInv )
        }
        ## Didn't find inverse in cache.  Calculate and store in cache.
        data <- x$get()
        matInv  <- solve(data, ...)
        x$setInverse (matInv )
        matInv 
}
