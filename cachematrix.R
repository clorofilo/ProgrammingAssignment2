##This functions search if the inverse of a matrix was previously calculated and, if it wasn't, it will be calculated 

## The makeCacheMatrix:
###1. set a matrix
###2. get a matrix
###3. set the inverse of a matrix
###4. get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function (y) {
    x<<- y
    m<<- NULL
  }
  get<- function () x
  setsolve<- function(solve) m<<- solve
  getsolve<- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function see if the inverse of a matrix was calculated and if it was not, it will calculate

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if (!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  data<- x$get()
  m<- solve(data, ...)
  x$setsolve(m)
  m
}
