## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	## initialisation
	m <- NULL
	## set the matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	## get the matrix
	get <- function() { 
		x
	}
	## set the inverse of the matrix
	setinversematrix <- function(inverse) { 
		m <<- inverse
	}
	## get the inverse of the matrix
	getinversematrix <- function() {
		m
	}
	## Lists of methods
	list(set=set, get=get, setinversematrix=setinversematrix, getinversematrix=getinversematrix)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinversematrix()
	## return the inverse if already exist
	if (!is.null(m)) {
		message("getting cached data")
		return (m)
	}
	## calculate the inverse
	data <- x$get()
	m <- solve(data,...) 
	x$setinversematrix(m)
	## return 
	m
}