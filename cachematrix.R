## These functions allow the creation and caching of the inverse matrix. By Caching	
## we can improve the performance of the computation when applied in looping 
## functions do

## This function creates a special list to pack the Matrix Inverse Functions

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	##This sets the matrix and clears the inverse variable
	get <- function() x 
	##This simply returns the matrix variable
	setInv <- function(MatrixInv) inv <<- MatrixInv
	## This simply sets the matrix inverse that has been computed externally
	getInv <- function() inv
	## This simply returns the precomputed inverse
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
		## this block checks to see whether we have already created the inverse - if so
		## we will indicate we have cached and then return the inverse matrix
		data <- x$get()
		inv <- solve(data, ...)
		x$setInv(inv)
		inv
		## If we haven't computed the inverse, we recover the matrix
		## then we compute the inverse matrix using solve
		## we then set the inverse in the cached variable which is checked in future computations
		## then we return the inverse matrix
}
