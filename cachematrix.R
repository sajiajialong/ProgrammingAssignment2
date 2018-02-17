## The first fn makeCacheMatrix is a list of function to create a matrix and store its inverse
## The second fn cacheSolve is to check if the inverse has already been stored. Otherwish calculate the 
## inverse and store it


## makeCacheMatrix is a list of functions to do the following things
## 1. set the value of a specific matrix y to x
## 2. print x
## 3. store the inverse of y to m
## 4. print the inverse

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    set<- function(y){
       x<<- y
       m<<- NULL
    }
  get<- function(y)x
  setinverse<- function(inverse) m<<- inverse
  getinverse<- function(inverse) m
  list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)     
}


## cacheSolve check if the inverse of y is already stored first. If not, it calculates
## the inverse and store it. If yes, the calculation part will be skipped

cacheSolve <- function(x, ...) {
       
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <-x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
