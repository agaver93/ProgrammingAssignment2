
#first, a function is created that initializes two objects, x and m
#x is formal argument and empty matrix
#m is set to null and will be used later to cache the matrix's inverse

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y) {
    x <<- y # y assigned to x in parent environment
    m <<- NULL # clears any previously cached value of m
  }
  get <- function() x 
  setInverse <- function(inverse) m <<- inverse 
  getInverse <- function() m 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
#the last section assigns each of the functions as an element in a list and
#returns in to the parent environment


#The below function calculates the inverse of the matrix created by the
#makeCacheMatrix above
#If the inverse has already been created, it will get the inverse
#from the cache

cacheSolve <- function(x, ...){
  #return a matrix that is the inverse of x
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
