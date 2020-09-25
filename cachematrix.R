## The first function take a basic matrix created in R
## Then create a lot of other function inside the environment
## so it can be calculated the inverse in each part
## the return of the first function is a list that contain the other functions
## so i can execute a function of the makeCacheMatrix environment from assign
## the function to a variable and select from the $ symbol

## With the second function it is calculated the inverse of elements that
## has been assigned to the previous list

## This function generates an environment with other functions that can be 
## execute from other function, for that is that is called 
## "makeCacheMatrix" unless it´s return is a list

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL  
  set <- function(y){ 
    x <<- y 
    inver <<- NULL}
  get <- function() x 
  setinver <- function(inverse) inver <<- solve 
  getinver <- function() inver 
  #all this function are in the environment of makeCacheMatrix
  return(list(set = set, get = get, 
              setinver = setinver,
              getinver = getinver))
}

##This function execute the segments of the previous list
##and return the inverse value of the matrix catched

cacheSolve <- function(x, ...) {        inver <- x$getinver() 
#if some data is in inver then the matrix inverse is already done
if(!is.null(inver)) { 
  me<-message("getting cached data") 
  return(me)
}
#this function will execute the solve function to the matrix
matrixin <- x$get() 
inver <- solve(matrixin, ...) 
x$setinver(inver) 
return(inver)
        
}

                