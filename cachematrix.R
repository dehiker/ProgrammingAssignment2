## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), it returns the inverse from the cache.
## If input matrix is NULL or non-invertible, numeric() is returned 

## Create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
		## Initialize
		inv <- NULL
		
		## Methods
		set <- function(y) {
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setInv <- function(i) inv <<- i
		getInv <- function() inv
		
		list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## it returns the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- mcm$getInv()
		if (!is.null(inv)) {
			message("getting cached data")
			return(inv)
		}
		
		mtx <- mcm$get()
		
		# Calculate inverse, return NULL if 'x' is not invertible
		ifelse(is.finite(determinant(x)$modulus), inv <- solve(x), inv <- NULL)
		mcm$setInv(inv)
		inv
}
