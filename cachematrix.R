##
## Calculates the inverse of matrix using a ginv function from MASS package library.
##
##

## This function saves the inverse of a matrix using cache system of objects in R
## i - is to save inverse matrix
## x - the matrix to inverse
##
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	} 
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(get = get, set = set, setinverse = setinverse, getinverse = getinverse) 
}


## This functions calculates the inverse of a matrix,
## in order to inverse the matrix you need the function ginv, 
## from the MASS package, so this function can install if no package
## found!.
##
## x - the matrix to inverse
## returns the inverse of matrix (x) 
cacheSolve <- function(x, ...) {
	if (require("MASS")) {
		print("MASS is installed and loaded correctly")
	} else {
		print("MASS must be installed")
		install.packages("MASS")
	}

	i <- x$getinverse()
	if (!is.null(i)) {
		print("matrix inverse is cached, getting ...")
		return(i)
	}
	print("matrix inverse is not cached, calculating ...")
	data <- x$get()
	i <- ginv(data, ...)
	x$setinverse(i)
	return(i)
}
	
