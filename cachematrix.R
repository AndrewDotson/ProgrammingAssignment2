## These functions are use to solve a the inverse of a matrix and
## store it for easy and fast recall

## makeCacheMatrix creates a matrix object for easy retreival and 
## storage of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) 
{
	cacheInverse  <- NULL
	set <- function(y)
	{
		x <<- y
		cacheInverse  <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) cacheInverse <<- inv
	getInverse <- function() cacheInverse
	list(set = set, get = get , setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve will, if one exists, return the stored matrix inverse
## if there is no matrix inverse, it will calculate, store, and 
## return the matrix inverse

cacheSolve <- function(x, ...) 
{
	## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
	if(!is.null(inverse))
	{
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setInverse(inverse)
	inverse
}