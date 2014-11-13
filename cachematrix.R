## makeCacheMatrix calls functions to set and get a matrix
## of values and pass them up to the global environment, so that they 
## can be passed back and forth between other functions-- in this case,
## cacheSolve, calls makeCacheMatrix data and functions inside of its
## local environment and allows the auxillery function (makeCacheMatrix) to
## write and read the data and pass it back in.

## makeCacheMatrix takes a matrix as argument and has the following 
# sub functions# 
# 1) set  sets a global variable x, to the value of the matrix passed to the function.
# and initializes the inv matrix to NULL.
# 2) get simply passes the matrix x 
# 3) setinverse passes the inv matrix (we tmp set it to  foo for clarity)
# to the global environment, so it can be utilized outside the environment.
# 4) getinverse passes the current value of inv
# return all these function results in a list with corresponding names.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(foo) inv <<- foo
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve, utilizes the power of lexical scoping and the superassignment
# operator to read, and write files to an auxiliary function that it calls 
# -- the argument x, that gets - passed is the auxillery function, makeCacheMatrix.
# the inv matrix is assigned by calling the function makeCacheMatrix, and setting to
# the getInverse subfunction of makeCacheMatrix.
# then the data is passed to data via x$get and inv is solved locally and x is
# set or has the set inverse inv value set to the local function return.
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
		
		}
