

# This function creates an R object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# This function requires an argument that is returned by makeCacheMatrix()
# in order to retrieve the mean from the cached value that is stored in the 
# makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

#For testing only
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2) #A simple matrix
myMatrix <- makeCacheMatrix(m1)
cacheSolve(myMatrix)
