#-----------------------------------------------------------------------------
# Calculates the inverse of a matrix
# ------------------------------------
# Has two functions
#
# 1. makeCacheMatrix: This function creates a special "matrix" object 
#			    that can cache its inverse.
#
# 2. cacheSolve: This function computes the inverse of the special "matrix" 
#		     returned by makeCacheMatrix above. If the inverse has already 
#		     been calculated (and the matrix has not changed), then the 
#		     cachesolve should retrieve the inverse from the cache.
#
# Assumption: The matrix supplied is always invertible.
#-----------------------------------------------------------------------------

# makeCacheMatrix  function creates a special "vector", which is really a list 
# containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

makeCacheMatrix <- function(x=matrix()){
	invmat<-NULL
	
	set <- function(y){
		x <<- y
		invmat <<- NULL
	}

	get <- function() x

	setinv <- function(inv) invmat <<- inv

	getinv <- function() invmat

	list(set = set, get =get,
	     setinv = setinv,
	     getinv = getinv)
}

# cacheSolve function computes the inverse of the matrix.
# Throws an error if the matrix supplied is not invertible.

cacheSolve <- function(x, ...) {
	invmat <- x$getinv()

	if(!is.null(invmat)) {
      	message("getting cached inverse.")
      	return(invmat)
      }
    
	data <- x$get()
	
	invmat <- solve(data)

	x$setinv(invmat)

	invmat
}

	