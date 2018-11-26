##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
	in <- NULL #Begins by setting the inverse to NULL as a place holder for future value
	set <- function(y) { 
		x <<- y 
		in <<- NULL 
	} 

get <- function() x # Returns the vector, x. This function takes advantage of lexical scoping features in R.
	#Since the symbol x is not defined within get(), R retrieves it from the parent environment of makeCacheMatrix()
	
	setin <- function(inverse) in <<- inverse
	getin <- function() in
	
	list(set = set, 
		get = get, 
		setin = setin, 
		getin = getin)

# Returns the special vector containing all of the functions just defined 
#This assigns these functions to an element within a list, and returns it to the parent environment
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) { 
  in <- x$getinv() #Function attempts to retrieve a inverse from the object passed in as argument.
		
		if (!is.null(in)) {
			message("getting cache data")
			return(in)
		} 
		#This checks to see whether the result is NULL. Since makeCacheMatrix() sets the cache inverse to NULL whenever a new vector is set into the object.
		#If the value is not NULL, we wil have a valid cache inverse to return to the parent environment
		
		data <- x$get()
		m <- solve(data, ...)
		x$setin(in)
		in
		#cacheSolve() is the only place where solve() function is executed, so makeCacheMatrix() is incomplete without cacheSolve()
 
}
