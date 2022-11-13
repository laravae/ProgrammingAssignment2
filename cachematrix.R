## Put comments here that give an overall description of what your
## functions do

#In this assignement we work on the concept of lexical scoping. 
#In R we can create functions that are in turn defined inside other functions. 
#In this case, the environment in which a function is defined is the body of another function. 

#So, in this assignment we created the makeCacheMatrix() function that is a kind of "constructor function" that has been used to construct other functions like the CacheSolve().

#Catching the inverse of a matrix:

#The *makeCacheMatrix* function creates a special "matrix" object that can cache its inverse, which is really a list containing a function to:  
#1.- set the the matrix
#2.- get the matrix
#3.- set the the inverse of a matrix
#4.- get the the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # Set the matrix
  set <- function(y) {
    x<<-y
    i <<- NULL
  }
  # Get the matrix
  get <- function() x
  # Set the the inverse of a matrix
  setinverse <- function(inverse) i <<- inverse
  # Get the the inverse of a matrix
  getinverse <- function() i
  #Return list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#The following function named *cacheSolve* computes the inverse of the special matrix returned by *makeCacheMatrix* above. If the inverse has already been calculated (and the matrix has not changed), then the *cachesolve* should retrieve the inverse from the cache. To get the inverse of a matrix, this functions uses the functions created in makeCacheMatrix.


cacheSolve <- function(x, ...) {
  # Return inverse of a square matrix (the inverse of 'x') created by above function
  i <- x$getinverse()
  #If inverse has already been calculated then return the inverse stored in the cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #Get x from our object
  data <- x$get()
  #Compute the inverse of a square matrix from our object
  i <- solve(data, ...)
  x$setinverse(i)
  #Return inverse of a square matrix
  i 
}
