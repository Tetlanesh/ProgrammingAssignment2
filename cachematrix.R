##this set of two functions is used to calculate and store in cache value of inverted matrix to reduce computation time when the inverted matrix is needed 
##and to recalculate only when calculation cache is not available

##function that stores both matrix and its inversed variant in cache
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL                                        #setting im (inversed matrix) as null - only used when calling main makecachematrix function
  
  set <- function(y) {                              #function (object method) for setting matrix that will be inversed in cache
    x <<- y                                         #setting matrix to variable x that can be accesed outside of this function (scoping)
    im <<- NULL                                     #reseting inversed matrix value (as matrix itself have been changed) - again with scoping
  }
  
  get <- function() x                               #another function (method) that returns value of our matrix from variable x (from cache)
  
  setinverse <- function(inverse) im <<- inverse    #function (object method) for setting inversed matrix in cache - with scoping
  
  getinverse <- function() im                       #function (object method) that returns value of cached inverted matrix
  
  list(set = set, get = get,                        #list that is being returned by main function containg four functions (set, get, setinverse, getinverse)
       setinverse = setinverse,
       getinverse = getinverse)
}



##function that checks if there is cached inverted matrix stored and returns it, or recalculates it if not already in cache and store result in cache
cacheSolve <- function(x, ...) {
  im <- x$getinverse()                              #inverse matrix gets the value from the cache
  
  if(!is.null(im)) {                                #we are checking if cached matrix is NULL or is it avtually there, if it is NOT NULL we do the followig:
    message("getting cached data")                  #writing info about getting velue from cache on console
    return(im)                                      #returns the invertedmatrix value and stops function execution
  }
  
  data <- x$get()                                   #this is only executed if previous condition was FALSE (im is NULL). We are getting original matrix from cache
  im <- solve(data, ...)                            #im get the value of inverted(data) - its a function for inverted matrix
  x$setinverse(im)                                   #store the result of invertion in chache
  im                                                #returns inverted matrix as result of the function
}


## EXAMPLE OF USAGE:
#
# set.seed(77)
# m <- matrix(sample.int(100,size=25,replace=TRUE), nrow=5)
# d <- makeCacheMatrix(m)
# cacheSolve(d) #may take a while depending on PC specs
# cacheSolve(d) #again to see that the cache value is taken instead of recalculating
 
 

