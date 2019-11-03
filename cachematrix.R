## Ankurjha17 - Coursera - R Programming - Assignment 2

## Functions and their descriptions:

#-------------------------------------------------------------------
## - makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
#-------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {

	z <- NULL # Define the function
	set <- function(y) 
	{
		x <<- y # Value setting
		z <<- NULL # Cache Clearing
	}
	get <- function() x
	
	setInverse <- function(inverse) z <<- inverse # Defining function to get the inverse
	getInverse <- function() z 	# List with functions returns
	
	list(	
			set = set, get = get,
			setInverse = setInverse,
			getInverse = getInverse)
	
}

#-------------------------------------------------------------------
## - cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
#-------------------------------------------------------------------

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
		
	z <- x$getInverse()
	
	if(!is.null(z)) 
	{
		message("extracting cached data")
		return(z)
	}
	data <- x$get() # Getting the value of matrix
	z <- solve(data) # Inverse calculation
	
	x$setInverse(z)
	z # Prints Inversed matrix
}

#-------------------------------------------------------------------