#The code below comprises of a couple of functions to 
#cache the Inverse of a matrix

#The function makeCachematrix creates a special "matrix" object that can cache its inverse

makeCachematrix <- function(x=matix()){
  m <- NULL
  set <- function(y) { #changes / updates the vector stored
    x <<- y
    m <<- NULL
  }
  get <- function() x #Returns x from function
  sinverse <- function(solve) m<<- solve
  ginverse <- function() m
  list(set = set, get=get, sinverse=sinverse, ginverse=ginverse)
}

#writing the cache solve function based on the cachemean example provided

#The Function cacheSolvecomputes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  m <- x$ginverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$sinverse(m)
  m
}