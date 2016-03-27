## This R file is to create a special matrix i.e cache matrix which retains its values 
##so as to give a matrix that is inverse of matrix

## makecacheMatrix creates a special "matrix" object that can cache its data as well as its inverse data
makeCacheMatrix <- function(x = matrix()) {
  ## returns: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ## this list is used as a input to cachesolve() function
  
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
    
  }
  
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## This function solves the matrix that is gives the inverse of x where x is a matrix

cacheSolve <- function(x, ...) {
  
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  ## the line below checks if the value has 
  ## already been cached or not and if cached, 
  ## it doesnt compute again thus saving time of re inisiating data
  
  if (!is.null(inv)){
    
    message("getting cached data")
    return(inv)
    
  }
  
  # if there is no cache data, we compute it here
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  
  x$setinv(inv)
  return(inv)
}


## to see if the caching really works as we expect..
## we calculate the duration for cache solve
## for the first time we make a cache solve and then again
## first time we call function cachesolve(), it takes more time as we do not have any previous data
## running cachesolve() again, the duration is much more less than previous
## thus confirming the cachesolve


test <- function (matrix){
  
  temp <- makeCacheMatrix(matrix)
  start.time <- Sys.time()
  cacheSolve(temp)
  dur <- Sys.time()- start.time
  print(dur) ## print duration for the first time we do a cache solve
  
  start.time <- Sys.time()
  cacheSolve(temp)
  dur <- Sys.time()- start.time
  print(dur) ## print duration for the second time we do a cache solve, and uses cached data
  
}
