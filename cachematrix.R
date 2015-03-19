## These functions allow the creation and caching of the inverse matrix. By Caching	
## we can improve the performance of the computation when applied in looping 
## functions do

## This function creates a special list to pack the Matrix Inverse Function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(MatrixInv) inv <<- MatrixInv 
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function performs the cache matrix inverse function using solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInv()
		if (!is.null(inv)){
			message("Retrieving Cached Inverse Matrix")
			return(inv)
		}
		data <- x$get()
		inv <- solve(data, ...)
		x$setInv(inv)
		inv
		
}
