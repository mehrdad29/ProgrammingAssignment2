## my functions caching the inverse of a matrix rather than compute it repeatedly.

## it takes an argument x of type matrix and it returns a list 
## with 4 list items (they are actually 4 functions wrapped in a list)
## if the inverse is not computed yet then i will be null and otherwise
## i is the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                                       # when makeCacheMatrix just called set i to Null        
  set <- function(y) {                            # this function set matrix
    x <<- y
    i <<- NULL                                    # i set to Null because matrix is just set and its inverse is not computed
  }
  get <- function() x                             # this function get matrix
  setInverse <- function(inverse) i <<- inverse  # this function set inverse of matrix
  getInverse <- function() i                      # this function get inverse of matrix
  list(set = set, get = get,                      # return the list that contains four functions
       setInverse = setInverse,
       getInverse = getInverse)
}


## the input is expecting a "special vector" made from makeCacheMatrix.
## the output is the inverse of the matrix coming whether from the special vector's 
## cache or computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()               # query the x vector's cache
  if(!is.null(i)) {                 # if there is a cache
    message("getting cached data")  
    return(i)                       # just return the cache, no computation needed
  }
  data <- x$get()                   # if there's no cache
  i <- solve(data, ...)             # we actually compute them here
  x$setInverse(i)                   # save the result back to x's cache
  i                                 # return the result
}

