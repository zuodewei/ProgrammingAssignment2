## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # 用于缓存逆矩阵

    set <- function(y) {
        x <<- y    # 重新设置矩阵
        inv <<- NULL  # 重置 inverse 缓存
    }
    get <- function() x

    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv

    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)  # 这里假定矩阵可逆
    x$setinverse(inv)
    inv
}

