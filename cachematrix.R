########################################################
## A matrix list (class) that caches the inverse
## input:
## 	myMatrix: a square invertible matrix
## return: 
##	A list (class) containing the methods
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse
##      4. get the inverse
########################################################
makeCacheMatrix <- function(myMatrix = matrix()) {

	## Initialise the inverse, it has not been calculated yet.
	## It will be calculated the first time it is referenced in cacheSolve.        
        myInverse = NULL

	## return the list
	list(
		set=	function(y) {
		                myMatrix <<- y
                		myInverse <<- NULL
		        }, 
		get=	function() myMatrix, 
		setinv=	function(inverse) myInverse <<- inverse, 
		getinv=	function() myInverse
	)
}

########################################################
## Solver for the matrix class that caches inverse
########################################################
cacheSolve <- function(myMatrix, ...) {
        
        myInverse = myMatrix$getinv()
        
        ## if the inverse has not been calculated then
        if (is.null(myInverse)){
		## 1. Calculate (solve) matrix
        	myInverse = solve(myMatrix$get(), ...)
		## 2. cache the inverse
	        myMatrix$setinv(myInverse)
        }
        ## return the inverse
        return(myInverse)
}