## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}

mat<- matrix(c(2,4,3,1,5,7,8,9),2,2)
mat
x <- makeCacheMatrix(mat)
x$get()
x$setinv(inv)
x$set(inv)

inv = x$getinv()
inv

mat.data = x$get()
mat.data

start.time = Sys.time()
dur = Sys.time() - start.time
inv = solve(mat.data, 2,2)
x$getinv()
print(dur)


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}

start.time = Sys.time()
cacheSolve(x)
dur = Sys.time() - start.time
print(dur)

  




