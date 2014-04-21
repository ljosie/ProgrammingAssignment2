## Below are two functions that are used to create a special object
## that stores a matrix and cache's its inverse. 

## The first function, makeCacheMatrix creates a special "matrix" object
## that can cache its inverse. The function is to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        # Initialize the value of stored inv to NULL
        inv <- NULL
        # 1. set the value of the matrix
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        # 2.get the value of the matrix
        get <- function() x
        # 3.set the value of the inverse
        setinverse <- function(inverse) inv <<- inverse
        # 4. get the value of the inverse
        getinverse <- function() inv
        # Return a list of all func above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The following function cacheSolve calculates the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        # If the value of inverse is already cached, get cache data
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cache data")
                return(inv)
        }
        # If not cached, calculate the inverse
        data <- x$get()
        inv <- solve(data, ...)
        # Then cache the inverse
        x$setinverse(inv)
        # Return a matrix that is the inverse of 'x'
        inv
}
