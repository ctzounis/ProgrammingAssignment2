## Following the Example: Caching the Mean of a Vector we can use  
#the first function makeCacheMatrix to create a list that contains
#a function to set the value of the matix, then get the value of
#the vector moreover, set the value of the function solve (that gives 
#the inverse of a matrix) and final get the value of the solve.

makeCacheMatrix <- function(x = matrix()) {
  inv_1 <- NULL
  set <- function(y) {
    x <<- y
    inv_1 <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv_1 <<- solve
  getsolve <- function() inv_1
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


#This function computes the inverse of the special 
#"matrix" returned by makeCacheMatrix above. 
#If the inverse is not NULL (as we did set it in the line 8), 
#then the cachesolve 
#should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv_1 <- x$getsolve()
  if(!is.null(inv_1)) {
    message("getting cached data")
    return(inv_1)
  }
  data <- x$get()
  inv_1 <- solve(data, ...)
  x$setsolve(inv_1)
  inv_1
}
