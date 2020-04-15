
## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() is a function which has functions to set, get the 
## values of the matrix whose inverse is to be calculated. It also has functions
## to retrive the inverse from the cache and push the calculated inverse value 
## to the cache. The function returns a list , which is a pointer to the functions.
## The function object can be used to access the functions in makeCacheMatrix()
## The set function also determines whether the matrix being set is same as the
## prev matrix using matequal() function. If the matrix being set is same
## as that whose inverse exists in the cache, then the inverse remains cached.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  curr <- x
  matequal <- function(a, b)
    is.matrix(a) && is.matrix(b) && dim(a) == dim(b) && all(a == b)
  set <- function(y) {
    if(!matequal(y,curr))
    {
      x <<- y
      i <<- NULL
      curr <<- y
    }  
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve() function calculates the inverse of the matrix if the 
## inverse has not been cached yet.THe inverse of the matrix is calculated
## if the determinant of the matrix is not zero.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  if(det(data)!=0)
  {
    i <- solve(data, ...)
    x$setinv(i)
  }
  else{
    print("Inverse does not exist")
  }
  i
}