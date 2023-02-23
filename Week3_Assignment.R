makeCacheMatrix <- function(x = matrix()){
  a <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  a <- x$getinverse()
  if (!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
}

B <- matrix(c(1,2,3,4),2,2)
B1 <- makeCacheMatrix(B)
B1
cacheSolve(B1)
