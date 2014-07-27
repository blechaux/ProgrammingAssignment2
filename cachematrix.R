## The makeCacheMatrix function creates
## a special matrix object.
## Then, the cacheSolve function will calculate
## the inverse of this matrix.
## If the inverse of this matrix has already been calculated, 
## the cacheSolve function will find it in the cache and
## return it, it will not have to calculate it.


## The makeCacheMatrix function creates
## a special matrix object.
makeCacheMatrix <- function(x = matrix()) {
inverse_x <- NULL
set <- function(y) {
x <<- y
inverse_x <<- NULL
                   }
get <- function() x
setinverse <- function(inverse) inverse_x <<-inverse
getinverse <- function() inverse_x
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
                                           }
 

## The cacheSolve function returns the inverse of a matrix
## created with the makeCacheMatrix function.
## If the cached inverse is available,
## the cacheSolve function retrieves it.
## If the cached inverse is not available,
## the cacheSolve function computes, caches and returns it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inverse_x <- x$getinverse()
if (!is.null(inverse_x)) {
message("getting cached inverse matrix")
return(inverse_x)
                         } 
else {
inverse_x <- solve(x$get())
x$setinverse(inverse_x)
return(inverse_x)
     }
                               }
