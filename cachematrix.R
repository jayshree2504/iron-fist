
## Creating makeCacheMatrix function which creates a list containing a fuction
## to get,set the value of the matrix,get,set the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
 get <- function() x
 setinverse <- function(inverse) i <<- inverse
 getinverse <- function() i
 list( set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
 }


## CacheSolve creates a function that gets the inverse of the matrix from the cached data.
## if the cached data returns null it gets the data from matrix x and create a function to get the inverse of the matrix x.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
