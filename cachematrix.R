#Below are two functions that can cache tke computations of the inverse of a matrix. 


#The function, `makeCacheMatrix` creates a special "matrix", which is in fact a list containing a function to set the value of the matrix , to get #the value the matrix, to set the value of the inverse and get the value of the inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#The second function evaluates the inversen of the special "matrix"
#created with the first function. It first checks to see if the
#inverse has already been computed. If so, it `get`s the inverse from the
#cache. Otherwise, it calculates the inverse of
#the matrix and sets the value of the inverse in the cache via the `setinv`
##function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
