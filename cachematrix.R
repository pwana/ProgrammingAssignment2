# makeCacheMatrix contains four functions.
# 1) set 2) get 3) setinverse 4)getinverse
# The four functions can be called as attribute properties of 
# the function that calls makeCacheMatrix.

# It takes as an argument, x = matrix, a matrix of data.



makeCacheMatrix <- function(x = matrix()) {
inv <- NULL #initialize inv to NULL (empty)

# set takes argument y, when called by makeCacheMatrix set attribute
# and sets global var x to the value of y that is passed in.
# inv is globally set to NULL ( both via superassignment operator)

set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x # get just returns var x to the function calling it
setinverse <- function(foo) inv <<- foo #setinverse will pass foo from global env
# and pass it to global environment. This allows calling function, to cache operations
# by setting the inverse (inv) variable globally and store it in memory.
getinverse <- function() inv # getinverse will call whatever inv is currently holding
# if it was set by setinverse from cachesolve, it returns the cashed matrix inv or
# if still empty returns NULL
list(set = set, get = get,
setinverse = setinverse,
getinverse = getinverse) # returns a list of all of the current functions and any
#variables they hold
}


# cacheSolve is essentially taking in the matrix variable x, with row, col properties
# the major cacheMatrix and matrix functions are called inside of this function and cached to global 
# memory using the cacheMatrix function. They are then able to be retrieved using the 
# same function. 

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
inv <- x$getinverse() # here we pass whatever is stored in x from prior iteration.
# it has to be set first by passing inv <- solve(data,...) and setting via x$setinv
if(!is.null(inv)) {
message("getting cached data")
return(inv)
} # the first time we run it will look to see if inv has been set previously and retrieve 
# it with a message "getting cached data," if it wasn't set yet, it returns NULL.
data <- x$get() # passes x from makeCacheMatrix to a variable called data
inv <- solve(data, ...) # sets inv to the solve function and sets inv <- solve(data=x) the first time
x$setinverse(inv) # here we pass our inv result to the temp variable foo in makeCacheMatrix
# which gets passed and cached to global memory via the superassignment operator in makeCacheMatrix.
inv # returns current value of inv from cacheSolve. 
}
