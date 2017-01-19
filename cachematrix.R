## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions cache the inverse of a matrix.

## The following function makes a special "matrix" object that can cache its 
## own inverse.
## 1. set = set the value of the matrix
## 2. get = get the value of the matrix
## 3. setinverse = set the value of the inverse of the matrix
## 4. getinverse = get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting inverse of the matrix")
        return(i)
    }
    my_matrix <- x$get()
    i <- solve(my_matrix)
    x$setinverse(i)
    i
}
