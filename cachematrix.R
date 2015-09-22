## Creating 2 functions.  One, makeCacheMatrix to create a matrix and 
## cache the inverse of the matrix, which is really a list of 4 functions.
## Two, cacheSolve to compute the inverse of the "matrix" returned by 
## the first function. If the inverse has already exists and the matrix 
## is the same, then cachesolve should pull the inverse from the cache value.
## Caching the inverse of the matrix should speed up processing time.


## This function creates a matrix and caches the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
                    
        inv <- NULL
        set <- function(y) {
                            
                x <<- y
                inv <<- NULL # start with matrix to null
        }
    
        get <- function() x # creates input matrix
        setinverse <- function(inverse) inv <<- inverse # inversed matrix
                                                        # passed to function
        getinverse <- function() inv # cached value of matrix
        list( set = set, 
              get = get,
              setinverse = setinverse,
              getinverse = getinverse
              ) # put all 4 functions in list
        
}


# Computes the inverse of the matrix returned by makeCacheMatrix above.
# If the inverse has been computed with no change to the matrix, 
# then it retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
        inv <- x$getinverse() # get value form cache & test in if empty
        if(!is.null(inv)) {
                message("getting cached inverted matrix")
                return(inv) # return inverted matrix from cache
        }
        
        # if cache is empty
        mat <- x$get()          # create the matrix
        inv <- solve(mat, ...)  # invert the matrix 
        x$setinverse(inv)       # send the inverted matrix to cache
        inv                     # return the inverted matrix 
}

