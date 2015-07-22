## makeCachematrix makes a special matrix which contains a list of four functions 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.setinverse  sets the inverse of matrix in cahe 
## 4.getinverse returns the inverse stored in cache


makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setinverse <- function(sol) cache <<-sol
  getinverse <- function() cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve uses the specially cretwd matrix by above function and then checks 
## whether there is an inverse already in cache if yes returns it, otherwise
## solve the inverse of the matrix and sets in the cache.

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
