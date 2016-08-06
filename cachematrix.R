## Programming Assignment 2 for Coursera MOOC "R Programming"


## The first function, makeCacheMatrix, creates an object of class "matrix", that can cache
## its inverse.


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
              x <<- y
              m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## The second function, cacheSolve, will return the inverse of the matrix object 
## returned by the function makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not been changes in between), the function
## cacheSolve should retrieve the inverse from cashe, instead of computing it anew.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
