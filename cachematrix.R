## A function to create a matrix with its inverse cached 
## This will reduce the computation effort where an inverse 
## has been calculated and cached before.

# Example:
# mat <- rbind(c(1, -1/4), c(-1/4, 1))  
# m <-  makeCacheMatrix(mat)
# m$get()
# [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00
# cacheSolve(m)
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# cacheSolve(m)
# getting cached data
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667

## creates a special "matrix" object that can cache its inverse
## where the value of the matrix can be set/get 
## and the value of its inverse can be set/get
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. 
## Otherwise the inverse will be calculated by solve() and be cached.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

